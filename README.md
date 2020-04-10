# conf

YAML configuration for Erlang applications.

The library provides a way to replace standard Erlang configuration
with equivalent YAML configuration. Besides more "admin friendly"
syntax, the library performs options validation, applies defaults,
and, if errors are found in the configuration file,
produces graceful user-friendly error messages.

Also note, since JSON is a subset of YAML, you can use the library
for processing JSON configuration files as well.

## Table of contents
- [Usage](#usage)
  - [Configuration reload](#configuration-reload)
- [Dependencies](#dependencies)
- [conf behaviour](#conf-behaviour)
  - [Example](#example)
- [Handling errors](#handling-errors)
  - [Reporting errors](#reporting-errors)

## Usage

It's probably better to describe the functionality of the library
on an example. For instance, the following Erlang configuration:
```erl
[
 {mnesia, [{dir, "/tmp/mnesia"},
           {auto_repair, true}]},

 {os_mon, [{disk_almost_full_threshold, 0.7},
           {system_memory_high_watermark, 0.9},
           {memsup_helper_timeout, 30},
           {disk_space_check_interval, 30}]},

 {crypto, [{rand_cache_size, 1024}]}
].
```
can be written in YAML configuration as:
```yaml
mnesia:
  dir: /tmp/mnesia
  auto_repair: true

os_mon:
  disk_almost_full_threshold: 70%
  system_memory_high_watermark: 90%
  memsup_helper_timeout: 30 sec
  disk_space_check_interval: 30 min

crypto:
  rand_cache_size: 1024
```
And instead of running Erlang VM as:
```sh
$ erl ... -config my_app.config
```
You run it as:
```sh
$ erl ... -conf file \"my_app.yml\"
```
Alternatively, if you use release generators, you can put the following into `sys.config`:
```erl
[
 {conf, [{file, "/path/to/my_app.yml"}]}
].
```
**NOTE**: the library can expand shell variables, so you can put something like
`"$HOME/config.yml"` in the above example.

You should also load `conf` application at startup. Please note that
`conf` application must be loaded right after `kernel` and `stdlib`, e.g. define `applications`
parameter of `my_app.app.src` file as:
```erl
{application, my_app,
 [...
  {applications,
    [kernel, stdlib, conf, ...]},
  ...
]}.
```
**NOTE**: Since the library only loads environment variables, their values are accessible
as usual via `application:get_env/2,3` functions.

### Configuration reload

Once the configuration file is loaded, you can use `conf:reload_file/0` to reload it:
```erl
-spec reload_file() -> ok | {error, error_reason()}.
```
The function loads configuration from the file defined in environment variable `file`.
On success it also applies `config_change/3` callback for the applications which
export it. The function is atomic in the sense that it only loads configuration if
it's valid. However, configuration change of loaded applications may fail because
`config_change/3` callback may fail for some applications.

The function returns `ok` if the configuration can be read and is valid, no matter whether
configuration change of loaded aplications has failed or not (however, warnings are
reported in this case). If the configuration cannot be read or is invalid,
`{error, Reason}` is returned. You can use function `conf:format_error/1` to print
the `Reason` in a human readable format:
```erl
-spec format_error(error_reason()) -> string().
```

## Dependencies

`conf` application relies heavily on [yval](https://github.com/zinid/yval)
application, so refer to its documentation for detailed information on how to write
YAML validators.

The library also depends on [fast_yaml](https://github.com/processone/fast_yaml)
parser.

## conf behaviour

In order to define YAML configuration for your application `my_app`, you should
create file `my_app_yaml.erl` inside your source directory (typically `src`).
The `conf` behaviour requires `validator/0` callback to be provided,
so your module `my_app` must define `validator/0` callback, that must return
`yval:options/2` validator. The parameters for `yval:options/2` validator
must at least contain `unique` option (which means that the validator will check for
duplicated options).

The library provides predefined YAML validators for built-in Erlang applications
such as `kernel`, `os_mon`, `crypto` and so on. You can use them as an example,
for instance, you can take a look at [mnesia_yaml.erl](src/mnesia_yaml.erl)
or [kernel_yaml.erl](src/kernel_yaml.erl).

### Example
Let's say your application `my_app` has 3 options:

- `choice` that accepts three atoms: `foo`, `bar` and `baz` with default
  being `foo`
- `timeout` that accepts timeout expressed in milliseconds or `infinity`,
  with default being `5` seconds
- `name` that accepts non empty binary with no default, because it's a
  mandatory option

The contents of `my_app_yaml.erl` will look as follows:
```erl
-module(my_app_yaml).
-behaviour(conf).

-export([validator/0]).
-import(yval, [options/2, enum/1, binary/0, non_empty/1, timeout/2]).

-spec validator() -> yval:validator().
validator() ->
    options(
      #{choice => enum([foo, bar, baz]),
        timeout => timeout(millisecond, infinity),
        name => non_empty(binary())},
      [unique,
       {required, [name]},
       {defaults,
        #{choice => foo,
          timeout => timer:seconds(5)}}]).
```
Then, if the configuration file `my_app.yml` is defined as:
```yaml
my_app:
  timeout: 10 sec
  name: Saturn
```
when loaded, `my_app` will have the following environment variables:
```erl
1> application:get_all_env(my_app).
[{timeout, 10000},
 {choice, foo},
 {name, <<"Saturn">>}]
```

## Handling errors

At application startup, if errors are detected in the configuration file, `conf`
application refuses to load. This is in conformance with "fail early" principle: Erlang
applications should not be running with misconfigured environment variables.

### Reporting errors

The underlying YAML validator is pretty clever and is also able to provide various hints
on what it expects to get. Consider a few examples.

Let's say we made a mistake in `my_app.yml` and instead of `foo` we set `foe`
as a value of `choice` option:
```yaml
my_app:
  choice: foe
  ...
```
When being loaded, the validator will produce the following error message:
> Failed to load configuration from /path/to/my_app.yml: Invalid value of parameter 'my_app->choice': Unexpected value: foe. Did you mean 'foo'? Possible values are: bar, baz, foo

If we made a mistake in an option name, e.g. we wrote `time out` instead of `timeout`, the validator
would produce the following error message:
> Failed to load configuration from /path/to/my_app.yml: Invalid value of parameter 'my_app': Unknown parameter: time out. Did you mean 'timeout'? Available parameters are: choice, name, timeout

If we forgot to define mandatory option `name`, the following error will be produced:
> Failed to load configuration from /path/to/my_app.yml: Invalid value of parameter 'my_app': Missing required parameter: name
