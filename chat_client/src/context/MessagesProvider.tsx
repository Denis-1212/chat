import { createContext, useContext } from "react";
import { useMessages } from "../hooks/useMessages";

const UseMessagesContext = createContext<ReturnType<typeof useMessages> | null>(null);

export const UseMessagesProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
    const { messages, sendChatMessage } = useMessages();

    return (
        <UseMessagesContext.Provider value={{ messages, sendChatMessage }}>
            {children}
        </UseMessagesContext.Provider>
    );
};

export const useMessagesContext = () => {
    const ctx = useContext(UseMessagesContext);
    if (!ctx) throw new Error('useMessagesContext must be used within WebSocketProvider');
    return ctx;
};