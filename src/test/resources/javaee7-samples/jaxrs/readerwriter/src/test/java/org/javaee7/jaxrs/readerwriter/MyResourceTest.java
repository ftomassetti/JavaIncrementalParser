/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package org.javaee7.jaxrs.readerwriter;

import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.WebTarget;

import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.arquillian.junit.Arquillian;
import org.jboss.arquillian.test.api.ArquillianResource;
import org.jboss.shrinkwrap.api.ShrinkWrap;
import org.jboss.shrinkwrap.api.spec.WebArchive;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.*;

import org.junit.runner.RunWith;

/**
 * @author Arun Gupta
 */
@RunWith(Arquillian.class)
public class MyResourceTest {

    Client client;
    WebTarget target;
    
    @ArquillianResource
    URL base;

    @Before
    public void setUp() throws MalformedURLException {
        client = ClientBuilder.newClient();
        client.register(MyWriter.class);
        target = client.target(URI.create(new URL(base, "webresources/fruits").toExternalForm()));
    }

    @After
    public void tearDown() {
        client.close();
    }

    @Deployment(testable = false)
    public static WebArchive createDeployment() {
        WebArchive war = ShrinkWrap.create(WebArchive.class)
                .addClasses(
                        MyApplication.class, 
                        MyResource.class,
                        MyObject.class,
                        MyReader.class,
                        MyWriter.class);
        
        System.out.println(war.toString(true));
        return war;
    }

    /**
     * Test of postFruit method, of class MyResource.
     */
    @Test
    public void testPostWithCustomMimeType() {
        String fruit = target
                .request()
                .post(Entity.entity(new MyObject(1), MyObject.MIME_TYPE), String.class);
        assertEquals("banana", fruit);
    }

    /**
     * Test of postFruitIndexed method, of class MyResource.
     */
    @Test
    public void testPostSimple() {
        String fruit = target
                .path("index")
                .request()
                .post(Entity.text("1"), String.class);
        assertEquals("banana", fruit);
    }

}
