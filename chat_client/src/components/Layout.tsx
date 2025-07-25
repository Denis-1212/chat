import LeftPanel from './LeftPanel';
import RightPanel from './RightPanel';
import styles from '../styles/Layout.module.scss';
import IncomingCallModal from './IncomingCallModal';

const Layout: React.FC = () => {
  return (
    <div className={styles.layout}>
      <LeftPanel />
      <IncomingCallModal />
      <RightPanel />
    </div>
  );
};

export default Layout;
