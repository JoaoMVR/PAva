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
				treatAnnotations(other, ann);
			}
		}
	}
	
	public void treatAnnotations(CtConstructor ctCons, KeywordArgs annotation){
		String annotationValue = annotation.value();
		
		
		String template ="template goes here xD";
		
	}

}
