import { UserStatus } from './UserStatus';

export interface UserItemProps {
  name: string;
  status: UserStatus;
  selected: boolean;
  onCall: (name: string) => void;
  onSelect: (name: string) => void;
}
