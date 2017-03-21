package ist.meic.pa;

import javassist.CannotCompileException;
import javassist.ClassPool;
import javassist.NotFoundException;
import javassist.Translator;

public class ConstructorTranslator implements Translator {

	@Override
	public void start(ClassPool arg0) throws NotFoundException, CannotCompileException {
		//does nothing
	}
	
	@Override
	public void onLoad(ClassPool arg0, String arg1) throws NotFoundException, CannotCompileException {
		//this is the method that will modify each class in the classpool
		//ClassPool.get();
	}

}
