import { useEffect, useRef, useState } from 'react';
import { useWebSocketContext } from '../context/WebSocketContext';
import Controllers from '../dto/enums/Controllers';
import RtcSignalingType from '../dto/enums/RtcSignalingType';
import Actions from '../dto/enums/Actions';

export function useCall() {
    const [isCalling, setIsCalling] = useState(false);
    const [callee, setCallee] = useState<string | null>(null);
    const [incomingCall, setIncomingCall] = useState<{ from: string; sdp: RTCSessionDescriptionInit } | null>(null);

    const ringtoneRef = useRef<HTMLAudioElement | null>(null);

    const pcRef = useRef<RTCPeerConnection | null>(null);
    const localStreamRef = useRef<MediaStream | null>(null);
    const { sendMessage, socket } = useWebSocketContext();

    const STUN_CONFIG = {
        iceServers: [{ urls: 'stun:stun.l.google.com:19302' }]
    };

    const startCall = async (target: string) => {
        const pc = new RTCPeerConnection(STUN_CONFIG);
        pcRef.current = pc;
        setCallee(target);

        const stream = await navigator.mediaDevices.getUserMedia({ audio: true });
        stream.getTracks().forEach((track) => pc.addTrack(track, stream));
        localStreamRef.current = stream;

        pc.onicecandidate = (event) => {
            if (event.candidate) {
                sendMessage(Controllers.SignalingController, Actions.IceCandidate, { type: RtcSignalingType.IceCandidate, to: target, candidate: event.candidate });
            }
        };

        pc.ontrack = (event) => {
            const audio = new Audio();
            audio.srcObject = event.streams[0];
            audio.autoplay = true;
        };

        const offer = await pc.createOffer();
        await pc.setLocalDescription(offer);

        sendMessage(Controllers.SignalingController, Actions.Offer, { type: RtcSignalingType.Offer, to: target, sdp: offer });
        setIsCalling(true);
    };

    const handleSignalingMessage = async (msg: any) => {
        switch (msg.type) {
            case RtcSignalingType.Offer:
                setIncomingCall({ from: msg.from, sdp: msg.sdp });

                // Воспроизводим сигнал
                ringtoneRef.current = new Audio('/sounds/ringtone.mp3');
                ringtoneRef.current.loop = true;
                ringtoneRef.current.play().catch(console.error);

                break;

            case RtcSignalingType.Answer:
                await pcRef.current?.setRemoteDescription(new RTCSessionDescription(msg.sdp));
                break;

            case RtcSignalingType.IceCandidate:
                if (msg.candidate) {
                    await pcRef.current?.addIceCandidate(new RTCIceCandidate(msg.candidate));
                }
                break;

            case RtcSignalingType.EndCall:
                endCall();
                break;
        }
    };

    const acceptCall = async () => {
        if (!incomingCall) return;
        console.log('acceptCall');
        if (ringtoneRef.current) {
            ringtoneRef.current.pause();
            ringtoneRef.current.currentTime = 0;
            ringtoneRef.current = null;
        }

        const pc = new RTCPeerConnection(STUN_CONFIG);
        pcRef.current = pc;

        const stream = await navigator.mediaDevices.getUserMedia({ audio: true });
        stream.getTracks().forEach((track) => pc.addTrack(track, stream));
        localStreamRef.current = stream;

        pc.onicecandidate = (event) => {
            if (event.candidate) {
                sendMessage(Controllers.SignalingController, Actions.IceCandidate, {
                    type: RtcSignalingType.IceCandidate,
                    to: incomingCall.from,
                    candidate: event.candidate
                });
            }
        };

        pc.ontrack = (event) => {
            const audio = new Audio();
            audio.srcObject = event.streams[0];
            audio.autoplay = true;
        };

        await pc.setRemoteDescription(new RTCSessionDescription(incomingCall.sdp));
        const answer = await pc.createAnswer();
        await pc.setLocalDescription(answer);

        sendMessage(Controllers.SignalingController, Actions.Answer, {
            type: RtcSignalingType.Answer,
            to: incomingCall.from,
            sdp: answer
        });

        setIsCalling(true);
        setCallee(incomingCall.from);
        setIncomingCall(null);
    };


    const rejectCall = () => {
        if (!incomingCall) return;
        console.log('rejectCall');
        if (ringtoneRef.current) {
            ringtoneRef.current.pause();
            ringtoneRef.current.currentTime = 0;
            ringtoneRef.current = null;
        }

        sendMessage(Controllers.SignalingController, Actions.EndCall, {
            type: RtcSignalingType.EndCall,
            to: incomingCall.from
        });
        setIncomingCall(null);
    };

    const endCall = () => {
        pcRef.current?.close();
        localStreamRef.current?.getTracks().forEach((t) => t.stop());
        pcRef.current = null;
        setIsCalling(false);
        setCallee(null);
    };

    useEffect(() => {
        if (!socket) return;
        const handler = (e: MessageEvent) => {
            try {
                const msg = JSON.parse(e.data);
                handleSignalingMessage(msg);
            } catch (err) {
                console.error('Invalid signaling message:', e.data);
            }
        };
        socket.addEventListener('message', handler);
        return () => socket.removeEventListener('message', handler);
    }, [socket]);

    return {
        startCall,
        endCall,
        isCalling,
        callee,
        incomingCall,
        acceptCall,
        rejectCall,

    };

}
