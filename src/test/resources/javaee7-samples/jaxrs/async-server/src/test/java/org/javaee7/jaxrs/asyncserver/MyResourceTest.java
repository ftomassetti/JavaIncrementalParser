/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package org.javaee7.jaxrs.asyncserver;

import static org.junit.Assert.assertEquals;

import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.WebTarget;

import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.arquillian.junit.Arquillian;
import org.jboss.arquillian.test.api.ArquillianResource;
import org.jboss.shrinkwrap.api.ShrinkWrap;
import org.jboss.shrinkwrap.api.spec.WebArchive;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * @author Arun Gupta
 */
@RunWith(Arquillian.class)
public class MyResourceTest {
    
    @Deployment(testable = false)
    public static WebArchive createDeployment() {
       return ShrinkWrap.create(WebArchive.class)
             .addClasses(MyApplication.class, MyResource.class);
    }

    private WebTarget target;

    @ArquillianResource
    private URL base;

    @Before
    public void setUpClass() throws MalformedURLException {
        Client client = ClientBuilder.newClient();
        target = client.target(URI.create(new URL(base, "webresources/fruits").toExternalForm()));
    }
    
    /**
     * Test of getList method, of class MyResource.
     */
    @Test
    public void testGetList() {
        String result = target.request().get(String.class);
        assertEquals("apple", result);
    }
    
}
