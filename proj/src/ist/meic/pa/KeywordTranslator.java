package ist.meic.pa;

import javassist.*;

import java.lang.annotation.Annotation;
import java.lang.reflect.*;

public class KeywordTranslator implements Translator {

    // FIXME: If it is empty why do we override this?
    @Override
    public void start(ClassPool arg0) throws NotFoundException,
                                             CannotCompileException {
        // Empty.
    }

    /**
     *  This is the method that will modify each class in the classpool.
     */
    // FIXME: Missing documentation.
    @Override
    public void onLoad(ClassPool pool, String className)
        throws NotFoundException, CannotCompileException {
        try {
            makeConstructor(pool.get(className));
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
    }

    // FIXME: This should be decoupled from the translator so that we can debug
    // the generated code by hand.
    // FIXME: Missing documentation.
    private void makeConstructor(CtClass ctClass)
        throws ClassNotFoundException, CannotCompileException {

        for(CtConstructor ctConstructor: ctClass.getConstructors()) {
            if (ctConstructor.getAnnotation(KeywordArgs.class) instanceof KeywordArgs) {
                final KeywordArgs ann =
                    (KeywordArgs) ctConstructor.getAnnotation(KeywordArgs.class);
                treatAnnotations(ctConstructor, ann);
            }
        }
    }

    // FIXME: This should be decoupled from the translator so that we can debug
    // the generated code by hand.

    /**
     * FIXME: Missing documentation.
     * IN
     *     @KeywordArgs("width=100,height=50,margin")
     *     public Widget(Object... args) {}

     * OUT
     *     public Widget(Object... args) {
     *       this.width = 100;
     *       this.height = 50;

     *       initialize set with class fields (probably not needed);
     *       assert args.length is even;

     *       for each (kword, value) in args:
     *         if field with name kword exists:
     *           this.'kword = value;
     *         else:
     *           throw new RuntimeException("unsuported kword");
     *     }
     * Instantiate like: new Widget("height", 10, "width", 2)
     */
    private void treatAnnotations(CtConstructor ctConstructor, KeywordArgs annotation)
        throws CannotCompileException {
        ctConstructor.setBody(makeTemplate(defaultAssignments(annotation)));
    }

    // FIXME: Missing documentation.
    // FIXME: Problems with this approach:
    //          1. Possibly two assignments to each field which may be a problem
    //             if the arguments (both to the annotation and to the constructor)
    //             are computationally intensive.
    //          2. Using reflection is significantly slower than hardcoded comparisons.
    //          3. This doesn't handle keywords that reference each other.
    //          4. Doesn't handle empty keywords.
    //          5. Doesn't handle inheritance.
    private String makeTemplate(String[] defaultAssignments) {
        String template = "{\n";

        for (String da : defaultAssignments) {
            // If da.equals("height=10") we get "this.height=10;".
            template += "    $0." + da + ";" + "\n";
        }

        template +=                                                                           "\n"
            + "    for (int i = 0, j = 1; j < $1.length; i += 2, j += 2) {" +                 "\n"
            + "        final String kword = (String) $1[i];" +                                "\n"
            + "        final Object value = $1[j];" +                                         "\n"
            +                                                                                 "\n"
            + "        try {" +                                                               "\n"
            + "            final java.lang.reflect.Field field = " +                          "\n"
            + "                $0.getClass().getDeclaredField(kword);" +                      "\n"
            + "            field.setAccessible(true);" +                                      "\n"
            + "            field.set($0, value);" +                                           "\n"
            + "        } catch (NoSuchFieldException e) {" +                                  "\n"
            + "            throw new RuntimeException(\"Unrecognized keyword: \" + kword);" + "\n"
            + "        } catch (Exception e) {" +                                             "\n"
            + "            throw new RuntimeException(e);" +                                  "\n"
            + "        }" +                                                                   "\n"
            + "    }" +                                                                       "\n";

        template += "}";
        return template;
    }

    /**
     * FIXME: Missing documentation.
     * defaultAssignments("width=100,height=50,margin") =
     *    ["width=100","height=50"]
     */
    private String[] defaultAssignments(KeywordArgs annotation) {
        // FIXME:TODO
        final String[] example = {"width=100","height=50","margin=5"};
        return example;
    }

}
