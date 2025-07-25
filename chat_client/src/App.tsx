import React from 'react';
import Layout from './components/Layout';
import { WebSocketProvider } from './context/WebSocketContext';
import { ClientsProvider } from './context/ClientsProvider';
import { UseMessagesProvider } from './context/MessagesProvider';
import { CallProvider } from './context/CallContext';

const App: React.FC = () => {
  return <WebSocketProvider>
    <ClientsProvider>
      <UseMessagesProvider>
        <CallProvider>
          <Layout />
        </CallProvider>
      </UseMessagesProvider>
    </ClientsProvider>
  </WebSocketProvider>;
};

export default App;
