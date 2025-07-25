import React from 'react';
import '../styles/IncomingCallModal.css'; // подключим стили отдельно
import { useCallContext } from '../context/CallContext';

const IncomingCallModal: React.FC = () => {
    const { incomingCall, acceptCall, rejectCall } = useCallContext();

    if (!incomingCall) return null;

    return (
        <div className="incoming-call-modal">
            <div className="modal-content">
                <h3>📞 Входящий звонок</h3>
                <p>От: <strong>{incomingCall.from}</strong></p>
                <div className="modal-actions">

                    <button className="accept-button" onClick={acceptCall}>Принять</button>
                    <button className="reject-button" onClick={rejectCall}>Отклонить</button>
                </div>
            </div>
        </div>
    );
};

export default IncomingCallModal;
