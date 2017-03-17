package pt.ist.ap.labs;

import java.lang.reflect.*;

public class RunTests {
   public static void main(String[] args) throws Exception {
	  Class classTests = Class.forName(args[0]);
      int passed = 0, failed = 0;
      for (Method m : classTests.getMethods()) {
         if (m.isAnnotationPresent(Test.class)) {
            try {
               Test test = m.getAnnotation(Test.class);
               String[] setups = test.value();
               invokeSetups(classTests, setups);
               m.invoke(null);
               passed++;
            } catch (Throwable ex) {
               System.out.printf("Test %s failed: %s %n", m, ex.getCause());
               failed++;
            }
         }
      }
      System.out.printf("Passed: %d, Failed %d%n", passed, failed);
   }
   
   public static void invokeSetups(Class classTests, String[] setups) {
	   int passed = 0, failed = 0, current = 0;
	   for(; current < setups.length; current++){
		   for (Method m : classTests.getMethods()) {
		         if (m.isAnnotationPresent(Setup.class) 
		        		 && m.getAnnotation(Setup.class).value().equals(setups[current])) {
		            try {
		               m.invoke(null);
		               passed++;
		            } catch (Throwable ex) {
		               System.out.printf("Test %s failed: %s %n", m, ex.getCause());
		               failed++;
		            }
		         }
		      }
	   }
   }
}