package ist.meic.pa;

import javassist.CannotCompileException;
import javassist.ClassPool;
import javassist.CtClass;
import javassist.CtConstructor;
import javassist.CtNewConstructor;
import javassist.NotFoundException;
import javassist.Translator;

import java.lang.annotation.Annotation;
import java.util.NoSuchElementException;
import java.util.stream.Stream;

public class KeywordTranslator implements Translator {

    @Override
    public void start(ClassPool arg0) throws NotFoundException, CannotCompileException {
        // Do nothing.
    }

    /**
     * Intercepts the KeywordArgs annotated constructor and alters its body.
     *
     * The behaviour is undefined if a class has more than one constructor
     * annotated with KeywordArgs, or an annotated constructor whose arguments
     * are not simply "Object...".
     */
    @Override
    public void onLoad(ClassPool pool, String className) throws NotFoundException, CannotCompileException {
        final CtClass clazz = pool.get(className);
        CtConstructor ctor;

        try {
            ctor = Stream.of(clazz.getConstructors())
                .filter(c -> c.hasAnnotation(KeywordArgs.class))
                .findFirst() // There should be zero or one, not more.
                .get();

            if (!Utils.hasDefaultConstructor(clazz)) {
                // Adds default constructor for inheritance instantiations.
                clazz.addConstructor(CtNewConstructor.defaultConstructor(clazz));
            }

            ctor.setBody(Template.build(clazz));
        } catch (ClassNotFoundException e) {
            throw new RuntimeException(e); // We have a bug if we reach this point.
        } catch (NoSuchElementException e) {
            return; // We have nothing to do.
        }
    }

} // class KeywordTranslator
