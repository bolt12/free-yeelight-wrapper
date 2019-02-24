# yeelight-wrapper

Yeelight Color Bulb command wrapper.

## Description

If you own a Yeelight Color Bulb, in your house you can use this wrapper to quickly issue commands from the terminal of your computer.

## Dependencies

- `ghc`
- `cabal`

## Installing

`> make compile`
`> sudo make install`

This will install the program on your computer. You can then makeee alias on your `.bashrc` like this:

`alias lightOn='ylwrapper on'`
`alias lightOff='ylwrapper off'`
`alias lightToggle='ylwrapper off'`

## Features

Right now this tool only supports:

- Turning the light *on*;
- Turning the light *off*;
- Toggling the light.

There's a lot more commands available in the [documentation](https://www.yeelight.com/download/Yeelight_Inter-Operation_Spec.pdf). 

Feel free to make a PR!
