import { useState, useRef } from 'react';
import Controllers from '../dto/enums/Controllers';
import Actions from '../dto/enums/Actions';
// import ClientInfo from '../dto/ClientInfo';
// import { ServerMessage } from '../dto/ServerMessage';
import { Payload } from '../dto/Payload';
// import ServerMessageType from '../dto/enums/ServerMessageType';

export function useWebSocket() {
    const [connected, setConnected] = useState(false);
    // const [messages, setMessages] = useState<{ id: string; username: string, message: string, datetime: string }[]>([]);
    // const [clients, setClients] = useState<{ id: string; username: string }[]>([]);
    // const [clientInfo, setClientInfo] = useState<ClientInfo>();
    const ws = useRef<WebSocket | null>(null);

    const connect = (username: string) => {
        if (ws.current?.readyState === WebSocket.OPEN) return;

        // ws.current = new WebSocket(`ws://${window.location.host}/websocket`);
        ws.current = new WebSocket(`ws://localhost:8080/websocket`);

        ws.current.onopen = () => {
            setConnected(true);
            // ws.current?.send(JSON.stringify({ type: 'client_init', username }));
            sendMessage(Controllers.UserController, Actions.Login, { username })
        };

        // ws.current.onmessage = (event) => {
        //     try {
        //         const raw_data = JSON.parse(event.data);
        //         handleServerMessage(raw_data);
        //     } catch {

        //         console.error('Invalid JSON received:', event.data);
        //     }
        // };

        ws.current.onclose = () => {
            setConnected(false);
            ws.current = null;
        };

        ws.current.onerror = () => {
            setConnected(false);
        };
    };

    function sendMessage<C extends keyof Payload, A extends keyof Payload[C]>(
        controller: C,
        action: A,
        payload: Payload[C][A]
    ) {
        if (ws.current?.readyState === WebSocket.OPEN) {
            ws.current.send(JSON.stringify({ controller, action, payload }));
        }
    }

    const disconnect = () => {
        ws.current?.close();
    };

    // const handleServerMessage = (data: ServerMessage) => {
    //     if (!isServerMessage(data)) {
    //         console.warn('Invalid message format received:', data);
    //         return;
    //     }
    //     switch (data.type) {
    //         case ServerMessageType.ClientInfo:
    //             setClientInfo({ id: data.client.id, username: data.client.username })
    //             break;
    //         case ServerMessageType.Clients:
    //             setClients(data.clients || []);
    //             break;
    //         case ServerMessageType.Messages:
    //             setMessages(data.messages || []);
    //             break;
    //         case ServerMessageType.SystemMessage:
    //             // setMessages((prev) => [...prev, data]);
    //             break;
    //         default:
    //             console.warn('Unknown message type:', data);
    //             break;
    //     }
    // };

    // function isServerMessage(data: any): data is ServerMessage {
    //     return data && typeof data === 'object' && 'type' in data && typeof data.type === 'string';
    // }

    return {
        connected,
        connect,
        disconnect,
        sendMessage,
        // messages,
        // clients,
        socket: ws.current,
        // clientInfo,
    };
}
