# free-yeelight-wrapper

Yeelight Color Bulb command wrapper.

## Description

If you own a Yeelight Color Bulb, in your house you can use this wrapper to quickly issue commands from the terminal of your computer.
You just need to enable *LAN Control* on the Yeelight App and check the IP.

## Dependencies

- `ghc`
- `stack`

## Installing

```bash
> cd free-yeelight-wrapper
> stack install
```

This will install the program on your computer. You can then make an alias on your `.bashrc` like this:

```
alias lightOn='free-yeelight on'
alias lightOff='free-yeelight off'
```

## Configuring

Create a file named `.yeelight-wrapper.config` on your home directory with the following format:

```
<HostName>
<Port>
```

Example:

```
192.168.1.6
55443
```

You should set the port to be _55443_.

_NOTE:_ You do not need to create this config file, by default `yeelight-wrapper` will
create one and in the first time you run it, it will try to discover your light bulb and
update the configuration file.

## Features

Right now this tool only supports:

- Turning the light *on*;
- Turning the light *off*;
- Toggling the light;
- Discovery protocol.

There's a lot more commands available in the [documentation](https://www.yeelight.com/download/Yeelight_Inter-Operation_Spec.pdf).

Feel free to make a PR!

## Troubleshooting

It might be the case that the light bulb is turned on, LAN control is enabled but the
program is not able to discover, or even connect to the light bulb and issue the commands.

If you are in this situation please be aware that it might be due to [IGMP snooping](https://forum.yeelight.com/t/bulb-stops-to-respond-to-ssdp-requests-after-some-minutes/702). To fix this, [disable IGMP snooping](https://oldwiki.archive.openwrt.org/doc/howto/udp_multicast).
