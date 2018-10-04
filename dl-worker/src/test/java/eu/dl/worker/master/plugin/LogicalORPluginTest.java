package eu.dl.worker.master.plugin;

import static eu.dl.core.ThrowableAssertion.assertThrown;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.matched.MasterablePart;
import eu.dl.worker.master.plugin.generic.LogicalORPlugin;

/**
 * LogicalORPlugin test class.
 * 
 * @author Tomas Mrazek
 */
public class LogicalORPluginTest {
    private final Foo master = new Foo();
    
    /**
     * Object for testing purposes.
     */
    public final class Foo implements MasterablePart {
        private Boolean foo;
        private String bar;
        
        /**
         * Default constructor. All properties are null.
         */
        Foo() {
        }
        
        /**
         * Constructor with foo initialization.
         *
         * @param foo 
         */
        Foo(final Boolean foo) {
            this.foo = foo;
        }
        
        /**
         * @return value of foo
         */
        public Boolean getFoo() {
            return foo;
        }

        /**
         * @param foo
         *      value to set
         */
        public void setFoo(final Boolean foo) {
            this.foo = foo;
        }

        /**
         * @return value of bar
         */
        public String getBar() {
            return bar;
        }

        /**
         * @param bar 
         *      value to set
         */
        public void setBar(final String bar) {
            this.bar = bar;
        }

        @Override public String getTenderId() {
            return null;
        }

        @Override public LocalDate getPublicationDate() {
            return null;
        }

        @Override public Foo setPublicationDate(final LocalDate publicationDate) {
            return this;
        }

        @Override
        public LocalDateTime getCreatedRaw() {
            return null;
        }
    }
    
    /**
     * Mastered NULL test.
     */
    @Test
    public final void nullTest() {
        LogicalORPlugin<Foo, Foo, Foo> plugin = new LogicalORPlugin<>(Arrays.asList("foo"));
        
        plugin.master(Arrays.asList(new Foo(), new Foo(), new Foo()), master,
                Arrays.asList(new Foo(), new Foo(), new Foo()));
        assertNull(master.getFoo());
    }
    
    /**
     * Mastered TRUE test.
     */
    @Test
    public final void trueTest() {
        LogicalORPlugin<Foo, Foo, Foo> plugin = new LogicalORPlugin<>(Arrays.asList("foo"));
        
        plugin.master(Arrays.asList(new Foo(false), new Foo(true), new Foo(true)), master,
                Arrays.asList(new Foo(false), new Foo(true), new Foo(true)));
        assertTrue(master.getFoo());
        
        plugin.master(Arrays.asList(new Foo(false), new Foo(true), new Foo(null)), master,
                Arrays.asList(new Foo(false), new Foo(true), new Foo(null)));
        assertTrue(master.getFoo());
    }
    
    /**
     * Mastered FALSE test.
     */
    @Test
    public final void falseTest() {
        LogicalORPlugin<Foo, Foo, Foo> plugin = new LogicalORPlugin<>(Arrays.asList("foo"));

        plugin.master(Arrays.asList(new Foo(false), new Foo(false), new Foo(false)), master,
                Arrays.asList(new Foo(false), new Foo(false), new Foo(false)));
        assertFalse(master.getFoo());
        
        plugin.master(Arrays.asList(new Foo(false), new Foo(null), new Foo(null)), master,
                Arrays.asList(new Foo(false), new Foo(null), new Foo(null)));
        assertFalse(master.getFoo());
    }
    
    /**
     * Unsupported functions test.
     */
    @Test
    public final void exceptionTest() {
        final List<Foo> foos = Arrays.asList(new Foo(false), new Foo(true), new Foo(null));
        
        //property 'bar' isn't defined as Boolean
        assertThrown(() -> (new LogicalORPlugin<Foo, Foo, Foo>(Arrays.asList("foo", "bar")))
            .master(foos, master, foos)).isInstanceOf(UnrecoverableException.class);
        
        //property 'unknown' doesn't exist
        assertThrown(() -> (new LogicalORPlugin<Foo, Foo, Foo>(Arrays.asList("unknown")))
            .master(foos, master, foos)).isInstanceOf(UnrecoverableException.class);
    }
}
