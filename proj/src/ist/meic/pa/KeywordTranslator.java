package ist.meic.pa;


import javassist.CannotCompileException;
import javassist.ClassPool;
import javassist.NotFoundException;
import javassist.Translator;
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
    private void makeConstructor(CtClass ctClass) throws ClassNotFoundException{
        for(CtConstructor other : ctClass.getConstructors()){
            if (other.getAnnotation(KeywordArgs.class) != null){
                KeywordArgs ann = (KeywordArgs) other.getAnnotation(KeywordArgs.class);
                treatAnnotations(ctClass, other, ann);
            }
        }
    }

    // FIXME: This should be decoupled from the translator so that we can debug
    // the generated code by hand.

    /**
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
    private void treatAnnotations(CtClass ctClass, CtConstructor ctCons, 
                                 KeywordArgs annotation){
        final String template = makeTemplate(defaultAssignments(annotation));

        // ...
    }

    private String makeTemplate(String[] defaultAssignments) {
        String template = "{";

        for (String kda : defaultAssignments) {
            // If kda.equals("height=10") we get "this.height=10;".
            template += "$0." + kda + ";";
        }

        template += "for (int i = 0, j = 1; j < $1.length; i += 2, j += 2) {"
            + "          final String kword = $1[i];"
            + "          final Object value = $1[j];"
            + ""
            + "          try {"
            + "              Class.forName($0.class).getField(kword).set($0, value);"
            + "          } catch (NoSuchFieldException e) {"
            + "              throw new RuntimeException(\"Unrecognized keyword: \" + kword);"
            + "          }"
            + "      }";

        return template;
    }


    /**
     * defaultAssignments("width=100,height=50,margin") = 
     *    ["width=100","height=50"]
     */
    private String[] defaultAssignments(KeywordArgs annotation) {
        // FIXME:TODO
        return null;
    }

}
