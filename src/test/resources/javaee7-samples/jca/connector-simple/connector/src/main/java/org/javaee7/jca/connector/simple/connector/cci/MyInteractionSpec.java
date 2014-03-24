/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.javaee7.jca.connector.simple.connector.cci;

import java.io.Serializable;
import javax.resource.cci.InteractionSpec;

/**
 *
 * @author arungup
 */
public class MyInteractionSpec implements InteractionSpec, Serializable {

    private final String functionName = "TRADE";
    private final int interactionVerb = InteractionSpec.SYNC_SEND_RECEIVE;
    private final int executionTimeout = 0;

    public String getFunctionName() {
        return functionName;
    }

    public int getInteractionVerb() {
        return interactionVerb;
    }

    public int getExecutionTimeout() {
        return executionTimeout;
    }
}
