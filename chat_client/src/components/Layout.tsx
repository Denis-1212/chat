import React from 'react';
import LeftPanel from './LeftPanel';
import RightPanel from './RightPanel';
import styles from '../styles/Layout.module.scss';

const Layout: React.FC = () => {
  return (
    <div className={styles.layout}>
      <LeftPanel />
      <RightPanel />
    </div>
  );
};

export default Layout;
