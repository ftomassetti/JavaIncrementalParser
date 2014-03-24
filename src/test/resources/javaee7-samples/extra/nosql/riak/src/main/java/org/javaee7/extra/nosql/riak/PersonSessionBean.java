/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2013 Oracle and/or its affiliates. All rights reserved.
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
package org.javaee7.extra.nosql.riak;

import com.basho.riak.client.IRiakClient;
import com.basho.riak.client.RiakException;
import com.basho.riak.client.RiakFactory;
import com.basho.riak.client.RiakRetryFailedException;
import com.basho.riak.client.bucket.Bucket;
import com.basho.riak.client.cap.UnresolvedConflictException;
import com.basho.riak.client.convert.ConversionException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.inject.Inject;
import javax.inject.Named;
import javax.inject.Singleton;

/**
 * @author Arun Gupta
 */
@Named
@Singleton
public class PersonSessionBean {

    @Inject
    Person person;

    Set<String> set = new HashSet<>();
    Bucket myBucket;

    @PostConstruct
    private void initDB() {
        try {
            IRiakClient client = RiakFactory.pbcClient("localhost", 8087);
            myBucket = client.fetchBucket("test").execute();
        } catch (RiakException ex) {
            Logger.getLogger(PersonSessionBean.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    @PreDestroy
    private void stopDB() {
    }

    public void createPerson() {
        try {
            myBucket.store(person.getName(), new Person(person.getName(), person.getAge())).execute();
            set.add(person.getName());
        } catch (RiakRetryFailedException | UnresolvedConflictException | ConversionException ex) {
            Logger.getLogger(PersonSessionBean.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    public List<Person> getPersons() {
        List<Person> persons = new ArrayList();
        for (String key : set) {
            try {
                Person p = myBucket.fetch(key, Person.class).execute();
                persons.add(p);
            } catch (UnresolvedConflictException | RiakRetryFailedException | ConversionException ex) {
                Logger.getLogger(PersonSessionBean.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
        return persons;
    }
}
