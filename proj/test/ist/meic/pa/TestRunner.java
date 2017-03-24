package ist.meic.pa;

import javassist.*;

import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;

import org.junit.Test;
import org.junit.runner.notification.RunNotifier;
import org.junit.runners.BlockJUnit4ClassRunner;
import org.junit.runners.model.InitializationError;

public class TestRunner extends BlockJUnit4ClassRunner {

    static ClassLoader customClassLoader;

    public TestRunner(Class<?> clazz) throws InitializationError {
        super(loadFromCustomClassloader(clazz));
    }

    // Loads a class in the custom classloader
    private static Class<?> loadFromCustomClassloader(Class<?> clazz)
        throws InitializationError {
        try {
            if (customClassLoader == null) {
                customClassLoader = new CustomClassLoader();
            }
            return Class.forName(clazz.getName(), true, customClassLoader);
        } catch (ClassNotFoundException e) {
            throw new InitializationError(e);
        }
    }

    public static class CustomClassLoader extends Loader {

        ClassLoader parent = null;

        public CustomClassLoader() {
            super();

            parent = Thread.currentThread().getContextClassLoader();

            try {
                addTranslator(ClassPool.getDefault(), new KeywordTranslator());
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }

        @Override
        public synchronized Class<?> loadClass(String name)
            throws ClassNotFoundException {

            if (name.startsWith("ist.meic.pa")) {
                return super.loadClass(name);
            } else {
                return parent.loadClass(name);
            }
        }

    }

}
