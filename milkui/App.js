import React from 'react';
import {
  SafeAreaView,
  View,
  StatusBar,
  Text,
} from 'react-native';
import styled from 'styled-components/native';

import { WSProvider } from './src/ws'
import CommandList from './src/CommandList'
import { COLORS } from './src/style'

const App = () => {
  return <WSProvider url={"ws://localhost:13579/websocket"}>
      <MainView>
      <StatusBar barStyle="dark-content" />
      <SafeAreaView>
        <CommandList/>
      </SafeAreaView>
    </MainView>
  </WSProvider>
};

const MainView = styled(View)`
  background-color: ${COLORS.bg};
  height: 100%;
`

export default App;
