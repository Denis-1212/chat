import { useEffect, useState } from "react";
import { useWebSocketContext } from "../context/WebSocketContext";
import ServerMessageType from "../dto/enums/ServerMessageType";
import Controllers from "../dto/enums/Controllers";
import Actions from "../dto/enums/Actions";
import { useClients } from "../context/ClientsProvider";


export function useMessages() {
    const { socket, sendMessage } = useWebSocketContext();
    const [messages, setMessages] = useState<{ id: string; username: string, message: string, datetime: string }[]>([]);
    const { clientInfo } = useClients();

    const sendChatMessage = (content: string) => {
        sendMessage(
            Controllers.MessageController,
            Actions.SendMessage,
            {
                content: content,
                datetime: getDateTimeString(),
                userid: clientInfo?.id,
                username: clientInfo?.username
            });
    };

    const getDateTimeString = (): string => {
        const now: Date = new Date();
        const year = now.getFullYear();
        const month = String(now.getMonth() + 1).padStart(2, '0');
        const day = String(now.getDate()).padStart(2, '0');
        const hours = String(now.getHours()).padStart(2, '0');
        const minutes = String(now.getMinutes()).padStart(2, '0');
        const seconds = String(now.getSeconds()).padStart(2, '0');

        const formatted = `${day}.${month}.${year} ${hours}:${minutes}:${seconds}`;
        return formatted;
    };

    const handleMessage = async (data: any) => {
        if (data.type === ServerMessageType.Messages) {
            setMessages(data.messages || []);
        }
    };

    useEffect(() => {
        if (!socket) return;
        const handler = (e: MessageEvent) => {
            try {
                const msg = JSON.parse(e.data);
                handleMessage(msg);
            } catch (err) {
                console.error('Invalid signaling message:', e.data);
            }
        };
        socket.addEventListener('message', handler);
        return () => socket.removeEventListener('message', handler);
    }, [socket]);

    return { messages, sendChatMessage };
}


