import React, { useEffect, useRef } from 'react';
import InputPanel from './InputPanel';
import styles from '../styles/RightPanel.module.scss';
import MessageItem from './message/MessageItem';
import { useWebSocketContext } from '../context/WebSocketContext';
import { useMessagesContext } from '../context/MessagesProvider';


const RightPanel: React.FC = () => {
  const { connected } = useWebSocketContext();
  const containerRef = useRef<HTMLDivElement | null>(null);
  const { messages } = useMessagesContext();

  useEffect(() => {
    const el = containerRef.current;
    if (el) {
      el.scrollTop = el.scrollHeight;
    }
  }, [messages]);

  if (connected) {
    return (
      <div className={styles.rightPanel}>
        <div className={styles.rightPanelContent} ref={containerRef}>
          {messages.map((msg, i) => (
            <MessageItem
              key={`${i}${msg.datetime}`}
              time={msg.datetime}
              username={msg.username}
              content={msg.message}
            />
          ))}
        </div>
        <InputPanel />
      </div>);
  }
  else {
    return (
      <InputPanel />);
  }

};

export default RightPanel;
