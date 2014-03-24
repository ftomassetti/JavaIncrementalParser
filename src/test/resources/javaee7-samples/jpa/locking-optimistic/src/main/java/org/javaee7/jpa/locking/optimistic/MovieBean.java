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
package org.javaee7.jpa.locking.optimistic;

import java.util.List;
import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.LockModeType;
import javax.persistence.PersistenceContext;

/**
 * @author Arun Gupta
 */
@Stateless
public class MovieBean {

    @PersistenceContext
    EntityManager em;
    
    public void addMovies() {
        Movie[] movies = new Movie[4];
        movies[0] = new Movie(1, "The Matrix", "Keanu Reeves, Laurence Fishburne, Carrie-Ann Moss");
        movies[1] = new Movie(2, "The Lord of The Rings", "Elijah Wood, Ian Mckellen, Viggo Mortensen");
        movies[2] = new Movie(3, "Inception", "Leonardo DiCaprio");
        movies[3] = new Movie(4, "The Shining", "Jack Nicholson, Shelley Duvall");
        for (Movie m : movies) {
            em.persist(m);
        }
    }

    public List<Movie> listMovies() {
        return em.createNamedQuery("Movie.findAll", Movie.class).getResultList();
    }

    public void updateMovie() {
        Movie m = em.find(Movie.class, 3);
//        em.lock(m, LockModeType.OPTIMISTIC);
        m.setName("INCEPTION");
        em.merge(m);
        em.flush();
    }
    
    public void deleteMovie() {
        Movie m = em.find(Movie.class, 1);
        em.remove(m);
        em.flush();
    }
    
}
