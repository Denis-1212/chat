import { useState } from 'react';
import styles from '../styles/LeftPanel.module.scss';
import UserItem from './user/UserItem';
import { UserStatus } from './user/UserStatus';
import { User } from './user/User';
import { useWebSocketContext } from '../context/WebSocketContext';
import classnames from 'classnames';
import { useCallContext } from '../context/CallContext';
import { useClients } from '../context/ClientsProvider';


const LeftPanel: React.FC = () => {
  const [selectedUser, setSelectedUser] = useState<string | null>(null);
  const { connected } = useWebSocketContext();
  const { startCall, endCall, isCalling } = useCallContext();
  const { clients } = useClients();

  // Обработчик для выбора пользователя
  const handleSelect = (name: string) => {
    setSelectedUser(name === selectedUser ? null : name); // Меняем состояние при клике
  };

  // Обработчик звонка 
  const handleCall = (targetUser: string) => {
    if (!isCalling) {
      startCall(targetUser);
    } else {
      endCall();
    }
  };

  const usersData: User[] = clients.map((client) => ({
    name: client.username,
    status: UserStatus.Online, // пока по умолчанию
  }));

  return (
    <div className={classnames(styles.leftPanel, !connected ? styles.hidden : '')}>

      {usersData.map((user, i) => (
        <UserItem
          key={`${i}${user.name}`}
          name={user.name}
          status={user.status}
          selected={user.name === selectedUser}
          onSelect={() => handleSelect(user.name)}
          onCall={() => handleCall(user.name)} />
      ))}

    </div>
  );
};

export default LeftPanel;
