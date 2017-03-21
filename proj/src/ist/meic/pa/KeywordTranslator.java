package ist.meic.pa;


import javassist.CannotCompileException;
import javassist.ClassPool;
import javassist.NotFoundException;
import javassist.Translator;
import javassist.*;

import java.lang.annotation.Annotation;
import java.lang.reflect.*;

public class KeywordTranslator implements Translator {

    @Override
    public void start(ClassPool arg0) throws NotFoundException, CannotCompileException {
        //does nothing
    }

    @Override
    public void onLoad(ClassPool pool, String className) throws NotFoundException, CannotCompileException {
        //this is the method that will modify each class in the classpool
        CtClass ctClass = pool.get(className);
        try {
            makeConstructor(ctClass);
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
    }

    public void makeConstructor(CtClass ctClass) throws ClassNotFoundException{
        CtConstructor[] cons = ctClass.getConstructors();
        for(CtConstructor other : cons){
            if (other.getAnnotation(KeywordArgs.class) != null){
                KeywordArgs ann = (KeywordArgs) other.getAnnotation(KeywordArgs.class);
                treatAnnotations(ctClass, other, ann);
            }
        }
    }

    public void treatAnnotations(CtClass ctClass, CtConstructor ctCons, KeywordArgs annotation){

        // IN
        //     @KeywordArgs("width=100,height=50,margin")
        //     public Widget(Object... args) {}

        // OUT
        //     public Widget(Object... args) {
        //       this.width = 100;
        //       this.height = 50;

        //       initialize set with class fields (probably not needed);
        //       assert args.length is even;

        //       for each (kword, value) in args:
        //         if field with name kword exists:
        //           this.'kword = value;
        //         else:
        //           throw new RuntimeException("unsuported kword");
        //     }

        // Instantiate like: new Widget("height", 10, "width", 2)

        // preprocess : String -> [String]
        // preprocess("width=100,height=50,margin") = ["width=100","height=50"]
        String[] annFieldNames = preprocess(annotation.value());
        String template = "{";

        for (String name : annFieldNames) {
            template += "$0." + f + ";";
        }

        CtField[] ctFields = ctClass.getFields();

        template += "for (int i = 0, j = 1; j < $1.length; i += 2, j += 2) {";

        // FIXME: Not yet generating what's supposed to.
        for (CtField f : ctFields) {
            template +=
                "if (\"" + f.getName() + "\".equals($1[i])) {"
                + "   $0." + f.getName() + " = $1[j];";
            + "}";
        }

        /* For the previous example this should generate:

           for (int i = 0, j = 1; j < args.length; i += 2, j += 2) {
             if ("height".equals(args[i])) {
               this.height = args[j]; <-- type error
               continue;
             }
             if ("width".equals(args[i])) {
               this.width = args[j]; <-- type error
               continue;
             }
             if ("margin".equals(args[i])) {
               this.margin = args[j]; <-- type error
               continue;
             }
             throw new RuntimeException("Unrecognized keyword: " + args[i]);
           }

        */

        template += "}";




    }

}
