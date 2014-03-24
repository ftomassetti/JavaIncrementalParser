package org.javaee7.websocket.chat;

import java.io.IOException;
import java.util.concurrent.CountDownLatch;

import javax.websocket.ClientEndpoint;
import javax.websocket.OnMessage;
import javax.websocket.OnOpen;
import javax.websocket.Session;

/**
 * @author Arun Gupta
 */
@ClientEndpoint
public class ChatClientEndpoint2 {
    public static String TEXT = "Client2 joins";
    public static CountDownLatch latch;
    public static String response;

    @OnOpen
    public void onOpen(Session session) {
        try {
            session.getBasicRemote().sendText(TEXT);
        } catch (IOException ioe) {
            ioe.printStackTrace();
        }
    }
    
    @OnMessage
    public void processMessage(String message) {
        response = message;
        latch.countDown();
    }
}
