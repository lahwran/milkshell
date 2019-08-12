###### Milkshell: Smooth computing. Just add milk!🥛 🍶


to run erlang portion for dev: `rebar3 shell`
right now, that will launch localhost:8080 saying "hello world". stay tuned...

mac setup:

1. install xcode via the [mac app store](https://apps.apple.com/us/app/xcode/id497799835?mt=12)
2. install android studio, see [react-native docs](https://facebook.github.io/react-native/docs/getting-started.html#1-install-android-studio)

```
# these adapted from react native getting-started: https://facebook.github.io/react-native/docs/getting-started.html
brew install yarn
brew install watchman
brew tap AdoptOpenJDK/openjdk
brew cask install adoptopenjdk8 # has to be jdk8!
yarn global add react-native-cli
yarn global add expo-cli

# if you want to work on the react-native-macos ui; note that react-native-macos is deprecated
yarn global add react-native-macos-cli
```

uis:
- android: `cd milkui; react-native run-android`
- ios: `cd milkui; react-native run-ios`
- web: `cd milkui; expo start --web`
- mac: `cd *_rnmac; react-native-macos run-macos`

backend:
- todo

