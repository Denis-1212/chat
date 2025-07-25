import styles from '../../styles/UserItem.module.scss';
import { Phone } from 'lucide-react';
import { UserItemProps } from './UserItemProps';
import { UserStatus } from './UserStatus';

const UserItem: React.FC<UserItemProps> = ({ name, status, selected, onCall, onSelect }) => {
  return (
    <div
      className={`${styles.userItem} 
    ${status == UserStatus.Offline ? styles.offline : styles.online} 
    ${selected ? styles.selected : ''}`}
      onClick={() => onSelect(name)}>

      <span className={styles.name}>{name}</span>
      <button disabled={status == UserStatus.Offline ? true : false}
        className={styles.callButton}
        onClick={() => onCall(name)}>
        <Phone size={16} />
      </button>
    </div>
  );
};

export default UserItem;
