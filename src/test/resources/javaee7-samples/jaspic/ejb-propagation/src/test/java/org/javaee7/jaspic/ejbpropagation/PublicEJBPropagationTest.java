package org.javaee7.jaspic.ejbpropagation;

import static org.junit.Assert.assertTrue;

import java.io.IOException;

import org.javaee7.jaspic.common.ArquillianBase;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.arquillian.junit.Arquillian;
import org.jboss.shrinkwrap.api.spec.WebArchive;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.xml.sax.SAXException;

/**
 * This tests that the established authenticated identity propagates correctly from the web layer to a "public" EJB (an EJB
 * without declarative role checking).
 * 
 * @author Arjan Tijms
 * 
 */
@RunWith(Arquillian.class)
public class PublicEJBPropagationTest extends ArquillianBase {

    @Deployment(testable = false)
    public static WebArchive createDeployment() {
        return defaultArchive();
    }

    @Test
    public void testProtectedServletWithLoginCallingEJB() throws IOException, SAXException {
        
        String response = getFromServerPath("protected/servlet-public-ejb?doLogin");

        // Both the web (HttpServletRequest) and EJB (EJBContext) should see the same
        // user name.
        assertTrue(response.contains("web username: test"));
        assertTrue("Web has user principal set, but EJB not.", response.contains("EJB username: test"));
    }

}