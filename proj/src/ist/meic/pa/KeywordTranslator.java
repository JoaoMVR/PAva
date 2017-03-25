package ist.meic.pa;

import javassist.*;

import java.lang.annotation.Annotation;
import java.lang.reflect.*;
import java.util.*;
import java.util.stream.*;

public class KeywordTranslator implements Translator {

    @Override
    public void start(ClassPool arg0) throws NotFoundException, CannotCompileException {
        // Do nothing.
    }

    @Override
    public void onLoad(ClassPool pool, String className) throws NotFoundException, CannotCompileException {
        try {
            makeConstructor(pool.get(className));
        } catch (ClassNotFoundException e) {
            throw new RuntimeException(e);
        }
    }

    // FIXME: Missing documentation.
    private void makeConstructor(CtClass clazz) throws ClassNotFoundException, CannotCompileException, NotFoundException {
        CtConstructor ctor;

        try {
            // FIXME: Actually, there could be more constructors annotated with 
            // KeywordArgs (altough there shouldn't be).
            ctor = Stream.of(clazz.getConstructors())
                .filter(c -> c.hasAnnotation(KeywordArgs.class))
                .findFirst() // There will be zero or one, not more.
                .get();
        } catch (NoSuchElementException e) {
            return; // We have nothing to do.
        }

        // FIXME: Why do we really need this?
        // Adds new constructor for inheritance instantiations.
        clazz.addConstructor(CtNewConstructor.defaultConstructor(clazz));

        ctor.setBody(Template.build(clazz));
    }

} // class KeywordTranslator
