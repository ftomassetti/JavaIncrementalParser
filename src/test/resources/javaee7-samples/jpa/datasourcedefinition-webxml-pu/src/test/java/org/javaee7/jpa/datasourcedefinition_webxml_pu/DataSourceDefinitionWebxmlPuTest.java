package org.javaee7.jpa.datasourcedefinition_webxml_pu;

import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.List;

import javax.inject.Inject;

import org.javaee7.jpa.datasourcedefinition_webxml_pu.entity.TestEntity;
import org.javaee7.jpa.datasourcedefinition_webxml_pu.service.TestService;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.arquillian.junit.Arquillian;
import org.jboss.shrinkwrap.api.Archive;
import org.jboss.shrinkwrap.api.ShrinkWrap;
import org.jboss.shrinkwrap.api.spec.WebArchive;
import org.jboss.shrinkwrap.resolver.api.maven.Maven;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * This tests that a data source defined in web.xml can be used by JPA. 
 * <p>
 * The actual JPA code being run is not specifically relevant; any kind of JPA operation that
 * uses the data source is okay here. 
 * 
 * @author Arjan Tijms
 */
@RunWith(Arquillian.class)
public class DataSourceDefinitionWebxmlPuTest {
    
    private static final String WEBAPP_SRC = "src/main/webapp";

    @Inject
    private TestService testService;
    
    @Deployment
    public static Archive<?> deploy() {
        return ShrinkWrap.create(WebArchive.class)
                .addPackages(true, DataSourceDefinitionWebxmlPuTest.class.getPackage())
                .addAsResource("META-INF/persistence.xml")
                .addAsWebInfResource(resource("web.xml"))
                .addAsLibraries(Maven.resolver()
                    .loadPomFromFile("pom.xml")
                    .resolve("com.h2database:h2")
                    .withoutTransitivity()
                    .asSingleFile())
                ;
    }

    @Test
    public void insertAndQueryEntity() throws Exception {
        
        testService.saveNewEntity();
        
        List<TestEntity> testEntities = testService.getAllEntities();
        
        assertTrue(testEntities.size() == 1);
        assertTrue(testEntities.get(0).getValue().equals("mytest"));
    }
    
    private static File resource(String name) {
        return new File(WEBAPP_SRC + "/WEB-INF", name);
    }
}
