package pt.ist.ap.labs;

import java.lang.reflect.*;
import javassist.*;

public class RunTests {
   public static void main(String[] args) throws Exception {
      int passed = 0, failed = 0;
      
      Class classTests = Class.forName(args[0]);
      
      for (Method m : classTests.getDeclaredMethods()) {
    	  //System.out.println("Method: " + m.getName());
         if (m.isAnnotationPresent(Test.class)) {
            try {
               Test test = m.getAnnotation(Test.class);
               String[] setups = test.value();
               if(setups[0] == "*") {
            	   for(Method m1 : classTests.getDeclaredMethods()) {
            		   if (m1.isAnnotationPresent(Setup.class)) {
            			   m1.invoke(null);
            		   }
            	   }
               }
               //System.out.println("Method: " + m.getName());
               //System.out.println("Setups: " + setups[0]);
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
	   int passed = 0, failed = 0;
	   for(String current : setups){
		   for (Method m : classTests.getDeclaredMethods()) {
		         if (m.isAnnotationPresent(Setup.class) 
		        		 && m.getAnnotation(Setup.class).value().equals(current)) {
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