import { createContext, useContext, useEffect, useState, useCallback } from 'react';
import { useWebSocketContext } from './WebSocketContext';
import ServerMessageType from '../dto/enums/ServerMessageType';

const ClientsContext = createContext<{
    clients: any[],
    clientInfo?: any
}>({ clients: [] });

export const ClientsProvider = ({ children }: { children: React.ReactNode }) => {
    const { socket } = useWebSocketContext();
    const [clients, setClients] = useState([]);
    const [clientInfo, setClientInfo] = useState();

    const handleMessage = useCallback((data: any) => {
        if (data?.type === ServerMessageType.ClientInfo) {
            console.log('ClientInfo received!')
            setClientInfo(data.client);
        } else if (data?.type === ServerMessageType.Clients) {
            setClients(data.clients || []);
        }
    }, []);

    useEffect(() => {
        if (!socket) return;
        const handler = (e: MessageEvent) => {
            try {
                const msg = JSON.parse(e.data);
                handleMessage(msg);
            } catch (err) {
                console.error('Invalid message:', e.data);
            }
        };

        socket.addEventListener('message', handler);
        return () => socket.removeEventListener('message', handler);
    }, [socket, handleMessage]);

    return (
        <ClientsContext.Provider value={{ clients, clientInfo }}>
            {children}
        </ClientsContext.Provider>
    );
};

export const useClients = () => useContext(ClientsContext);
