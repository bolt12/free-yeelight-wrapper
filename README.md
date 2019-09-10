# free-yeelight-wrapper

Yeelight Color Bulb command wrapper.

## Description

If you own a Yeelight Color Bulb, in your house you can use this wrapper to quickly issue commands from the terminal of your computer.
You just need to enable *LAN Control* on the Yeelight App and check the IP.

## Dependencies

- `ghc`
- `stack`

## Installing

`> stack install free-yeelight-wrapper`

This will install the program on your computer. You can then make an alias on your `.bashrc` like this:

```
alias lightOn='free-yeelight-wrapper on'
alias lightOff='free-yeelight-wrapper off'
alias lightToggle='free-yeelight-wrapper off'
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

You should set the port to be 55443.

## Features

Right now this tool only supports:

- Turning the light *on*;
- Turning the light *off*;
- Toggling the light;
- Discovery protocol.

There's a lot more commands available in the [documentation](https://www.yeelight.com/download/Yeelight_Inter-Operation_Spec.pdf). 

Feel free to make a PR!
