package ist.meic.pa;

import javassist.*;

public class KeyConstructors {
	public static void main(String[] args){
		Translator translator = new KeywordTranslator(); //creates a translator for modifying constructors
		ClassPool pool = ClassPool.getDefault(); //gets classes
		Loader classLoader = new Loader();
		try {
			classLoader.addTranslator(pool, translator);
		} catch (NotFoundException | CannotCompileException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		try {
			classLoader.run(args[0], null);
		} catch (Throwable e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} //runs the Test provided as input

	}
}
