/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.javaee7.jaxrs.singleton;

import static org.junit.Assert.assertEquals;

import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.util.StringTokenizer;

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
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.MethodSorters;

/**
 * @author Arun Gupta
 */
@RunWith(Arquillian.class)
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class ApplicationSingletonResourceTest {

    @ArquillianResource
    private URL base;

    Client client;
    WebTarget target;

    @Before
    public void setUp() throws MalformedURLException {
        client = ClientBuilder.newClient();
        target = client.target(URI.create(new URL(base, "webresources/application").toExternalForm()));
    }

    @After
    public void tearDown() {
        client.close();
    }

    @Deployment(testable = false)
    public static WebArchive createDeployment() {
        return ShrinkWrap.create(WebArchive.class)
                .addClasses(
                        MyApplication.class,
                        ApplicationSingletonResource.class);
    }

    @Test
    public void test1Post() {
        target.request().post(Entity.text("pineapple"));
        target.request().post(Entity.text("mango"));
        target.request().post(Entity.text("kiwi"));
        target.request().post(Entity.text("passion fruit"));

        String list = target.request().get(String.class);
        StringTokenizer tokens = new StringTokenizer(list, ",");
        assertEquals(4, tokens.countTokens());
    }

    @Test
    public void test2Get() {
        String response = target.path("2").request().get(String.class);
        assertEquals("kiwi", response);
    }

    @Test
    public void test3Delete() {
        target.path("kiwi").request().delete();

        String list = target.request().get(String.class);
        StringTokenizer tokens = new StringTokenizer(list, ",");
        assertEquals(3, tokens.countTokens());
    }

    @Test
    public void test4Put() {
        target.request().put(Entity.text("apple"));

        String list = target.request().get(String.class);
        StringTokenizer tokens = new StringTokenizer(list, ",");
        assertEquals(4, tokens.countTokens());
    }

}
