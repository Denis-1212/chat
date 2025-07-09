import React from 'react';
import styles from '../../styles/MessageItem.module.scss';
import { MessageItemProps } from './MessageItemProps';

const MessageItem: React.FC<MessageItemProps> = ({ time, username, content }) => {

    return (
        <div className={styles.messageItem}>
            <div className={styles.messageHeader}>
                <span className={styles.username}>{username}</span>
                <span className={styles.time}>{time}</span>
            </div>
            <div className={styles.messageContent}>
                <p>{content}</p>
            </div>
        </div>
    );
};

export default MessageItem;
