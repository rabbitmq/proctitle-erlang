# Process title handling for Erlang

[![Build Status](https://travis-ci.com/rabbitmq/proctitle-erlang.svg?branch=master)](https://travis-ci.com/rabbitmq/proctitle-erlang)
[![Hex version](https://img.shields.io/hexpm/v/proctitle-erlang.svg "Hex version")](https://hex.pm/packages/proctitle-erlang)

This is a library which allows an Erlang application to change the
system process name of the Erlang VM as it appears in `ps(1)` and
`top(1)` for instance.

## Supported Operating Systems

| Operating system | API |
|------------------|-----|
| [FreeBSD](https://www.freebsd.org/) | [`setproctitle(3)`](https://www.freebsd.org/cgi/man.cgi?query=setproctitle&sektion=3) |
| Linux | [`prctl(2)`](http://man7.org/linux/man-pages/man2/prctl.2.html) |

## Project Maturity

This project is of beta quality.

## License and Copyright

See [LICENSE](./LICENSE).

© 2020 Pivotal Software, Inc.
