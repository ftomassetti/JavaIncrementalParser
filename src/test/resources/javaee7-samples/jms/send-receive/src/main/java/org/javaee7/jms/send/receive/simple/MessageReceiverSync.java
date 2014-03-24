/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2012 Oracle and/or its affiliates. All rights reserved.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License.  You can
 * obtain a copy of the License at
 * https://glassfish.dev.java.net/public/CDDL+GPL_1_1.html
 * or packager/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 *
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at packager/legal/LICENSE.txt.
 *
 * GPL Classpath Exception:
 * Oracle designates this particular file as subject to the "Classpath"
 * exception as provided by Oracle in the GPL Version 2 section of the License
 * file that accompanied this code.
 *
 * Modifications:
 * If applicable, add the following below the License Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * "Portions Copyright [year] [name of copyright owner]"
 *
 * Contributor(s):
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding "[Contributor]
 * elects to include this software in this distribution under the [CDDL or GPL
 * Version 2] license."  If you don't indicate a single choice of license, a
 * recipient has the option to distribute your version of this file under
 * either the CDDL, the GPL Version 2 or to extend the choice of license to
 * its licensees as provided above.  However, if you add GPL Version 2 code
 * and therefore, elected the GPL Version 2 license, then the option applies
 * only if the new code is made subject to such option by the copyright
 * holder.
 */
package org.javaee7.jms.send.receive.simple;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.annotation.Resource;
import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.jms.JMSContext;
import javax.jms.JMSException;
import javax.jms.Queue;
import javax.jms.QueueBrowser;

import org.javaee7.jms.send.receive.Resources;

/**
 * Synchronous message reception with container-managed JMSContext.
 * @author Arun Gupta
 */
@Stateless
public class MessageReceiverSync {

    @Inject
    private JMSContext context;
    
    @Resource(mappedName=Resources.SYNC_CONTAINER_MANAGED_QUEUE)
    Queue myQueue;

    public String receiveMessage() {
        return context.createConsumer(myQueue).receiveBody(String.class, 1000);
    }
    
    public void receiveAll() {
        System.out.println("--> Receiving redundant messages ...");
        try {
            QueueBrowser browser = context.createBrowser(myQueue);
            while (browser.getEnumeration().hasMoreElements()) {
                System.out.println("--> here is one");
                context.createConsumer(myQueue).receiveBody(String.class, 1000);
            }
        } catch (JMSException ex) {
            Logger.getLogger(MessageReceiverSync.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
}
