# Karabiner Elements profile switcher (Alfred Workflow)

The fresh macOS Sierra rewrite of the original Karabiner, the [Karabiner Elements](https://github.com/tekezo/Karabiner-Elements) qietly supports multiple profiles, which has been one of the core features, historically speaking. Although, as of now, you can define multiple profiles in the `~/.config/karabiner/karabiner.json` file, but you can't switch between them easily. This is an important features for people that regularly switch between internal / Apple keyboard and an external USB one, which usually have different layout, hence the need for 2+ profiles.

Karabiner Elements listens for changes in `karabiner.json` config file, and there is a setting that specifies which profile is _currently active_ – `selected: true`. 

This workflow does the following:

1. Type `keprofile` (you can change this if you like), to fetch your profiles defined in `karabiner.json` and list them in Alfred
2. Once you select a profile you want to switch to, hit ENTER and the workflow script will **edit your karabiner.json file** so that the selected profile becomes active

Note that after you switch to a different profile, you can adjust its settings inside Karabiner Elements preferences window, save them and it'll all work seamlessly; the workflow only changes `selected: true|false` attribute, nothing else.



## Commands

- `keprofile` - lists your profiles; then ENTER to switch to one



## Example

#### Workflow

![](http://i.imgur.com/8LN9mQI.png)



#### You can freely edit settings of each profile after switching to it

![](http://i.imgur.com/9rIV5jS.png)



#### Sample karabiner.json with multiple profiles

```
{
    "profiles": [
        {
            "devices": [
                {
                    "disable_built_in_keyboard_if_exists": false,
                    "identifiers": {
                        "is_keyboard": true,
                        "is_pointing_device": false,
                        "product_id": 610,
                        "vendor_id": 1452
                    },
                    "ignore": false
                },
                {
                    "disable_built_in_keyboard_if_exists": false,
                    "identifiers": {
                        "is_keyboard": true,
                        "is_pointing_device": false,
                        "product_id": 5890,
                        "vendor_id": 1241
                    },
                    "ignore": false
                }
            ],
            "fn_function_keys": {
                "f1": "vk_consumer_brightness_down",
                "f10": "mute",
                "f11": "volume_down",
                "f12": "volume_up",
                "f2": "vk_consumer_brightness_up",
                "f3": "f3",
                "f4": "vk_launchpad",
                "f5": "vk_consumer_illumination_down",
                "f6": "vk_consumer_illumination_up",
                "f7": "vk_consumer_previous",
                "f8": "vk_consumer_play",
                "f9": "vk_consumer_next"
            },
            "name": "USB Standard Keyboard",
            "selected": true,
            "simple_modifications": {
                "caps_lock": "escape",
                "escape": "caps_lock",
                "fn": "left_control",
                "left_command": "left_option",
                "left_option": "left_command"
            },
            "virtual_hid_keyboard": {
                "caps_lock_delay_milliseconds": 0,
                "keyboard_type": "ansi"
            }
        },
        {
            "devices": [
                {
                    "disable_built_in_keyboard_if_exists": false,
                    "identifiers": {
                        "is_keyboard": true,
                        "is_pointing_device": false,
                        "product_id": 610,
                        "vendor_id": 1452
                    },
                    "ignore": false
                },
                {
                    "disable_built_in_keyboard_if_exists": false,
                    "identifiers": {
                        "is_keyboard": true,
                        "is_pointing_device": false,
                        "product_id": 5890,
                        "vendor_id": 1241
                    },
                    "ignore": false
                }
            ],
            "fn_function_keys": {
                "f1": "vk_consumer_brightness_down",
                "f10": "mute",
                "f11": "volume_down",
                "f12": "volume_up",
                "f2": "vk_consumer_brightness_up",
                "f3": "f3",
                "f4": "vk_launchpad",
                "f5": "vk_consumer_illumination_down",
                "f6": "vk_consumer_illumination_up",
                "f7": "vk_consumer_previous",
                "f8": "vk_consumer_play",
                "f9": "vk_consumer_next"
            },
            "name": "Apple Keyboard",
            "selected": true,
            "simple_modifications": {
                "caps_lock": "escape",
                "escape": "caps_lock",
                "fn": "left_control"
            },
            "virtual_hid_keyboard": {
                "caps_lock_delay_milliseconds": 0,
                "keyboard_type": "ansi"
            }
        }
    ]
}
```





## Thanks to

- [@tekezo](https://github.com/tekezo) for creating an awesome key switcher application for OSX / macOS – [Karabiner Elements](https://github.com/tekezo/Karabiner-Elements).
- [@bennypowers](https://github.com/bennypowers) for a fresh Karabiner Elements logotype, as submitted in [this pull request](https://github.com/tekezo/Karabiner-Elements/pull/500/files).
- [@deanishe](https://github.com/deanishe) for creating an amazing, easy to use Alfred Workflow creation framework – [Alfred-Workflow](http://www.deanishe.net/alfred-workflow/index.html).
