import { createContext, useContext } from 'react';
import { useCall } from '../hooks/useCall';

const CallContext = createContext<ReturnType<typeof useCall> | null>(null);

export const CallProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
    const call = useCall();
    return (
        <CallContext.Provider value={call}>
            {children}
        </CallContext.Provider>
    );
};

export const useCallContext = () => {
    const ctx = useContext(CallContext);
    if (!ctx) throw new Error('useCallContext must be used within CallProvider');
    return ctx;
};
