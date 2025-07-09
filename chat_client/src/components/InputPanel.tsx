import React, { useEffect, useState } from 'react';
import styles from '../styles/InputPanel.module.scss';
import { useWebSocketContext } from '../context/WebSocketContext';
import { ArrowUp, Sun, Moon, UserPlus, UserMinus } from 'lucide-react';
import { UserCheck } from 'lucide-react';
import { useMessagesContext } from '../context/MessagesProvider';
import { useClients } from '../context/ClientsProvider';



const InputPanel: React.FC = () => {
  const [theme, setTheme] = useState('light');
  const [username, setUsername] = useState('');
  const [message, setMessage] = useState('');

  const { sendChatMessage } = useMessagesContext();
  const { clientInfo } = useClients();

  const {
    connected,
    connect,
    disconnect,
  } = useWebSocketContext();

  const handleConnect = () => {
    if (username.trim()) connect(username.trim());
  };

  useEffect(() => {
    document.documentElement.setAttribute('data-theme', theme);
  }, [theme]);

  const toggleTheme = () => {
    setTheme((theme) => (theme === 'dark' ? 'light' : 'dark'));
  };

  const sendMessage = () => {
    sendChatMessage(message.substring(0, 25));
    setMessage('')
  }

  return (
    <>
      <div className={styles.inputPanel}>

        {!connected ? (
          <div className={styles.inputRow}>
            <input
              value={username}
              onChange={(e) => setUsername(e.target.value.substring(0, 25))}
              placeholder="Введите имя"
              onKeyDown={(e) => {
                if (e.key === 'Enter') {
                  handleConnect();
                }
              }}
            />
            <button onClick={handleConnect}
              title="Подключиться"
            ><UserPlus /></button>
          </div>
        ) : (
          <div>
            <div className={styles.inputRow}>
              <button onClick={disconnect} title="Отключиться"><UserMinus /></button>
              <button onClick={toggleTheme} title={theme === 'dark' ? "Тёмная тема" : "Светлая тема"}>
                {theme === 'dark' ? <Moon /> : <Sun />}

              </button>
              <input
                value={message}
                onChange={(e) => setMessage(e.target.value)}
                placeholder="Введите сообщение"
                onKeyDown={(e) => {
                  if (e.key === 'Enter') {
                    sendMessage();
                  }
                }}
              />
              <button onClick={() => {
                sendMessage();
              }}
                title="Отправить сообщение"
              >
                <ArrowUp />
              </button>

            </div>
            <div className={styles.status}>
              <div className={styles.stausIcon}><UserCheck /></div>
              <h6>Вы подключились как : {clientInfo?.username}</h6>
            </div>
          </div>
        )}
      </div>
    </>
  );
};

export default InputPanel;
