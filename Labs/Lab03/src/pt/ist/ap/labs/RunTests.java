package pt.ist.ap.labs;

import java.lang.reflect.*;

public class RunTests {
	
   public static void main(String[] args) throws Exception {
	  int passed = 0, failed = 0;
	   
      Class<?> classTests = Class.forName(args[0]);
      
      for (Method m : classTests.getDeclaredMethods()) {
    	 m.setAccessible(true);
         if (m.isAnnotationPresent(Test.class)) {
            try {
               Test test = m.getAnnotation(Test.class);
               String[] setups = test.value();
               if(setups[0].equals("*")) {
            	   setups = new String[3];
            	   setups[0] = "s1";
            	   setups[1] = "s2";
            	   setups[2] = "s3";
               }
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
   
   public static void invokeSetups(Class<?> classTests, String[] setups) throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
	   for(String current : setups){
		   for (Method m : classTests.getDeclaredMethods()) {
			   	 m.setAccessible(true);
		         if (m.isAnnotationPresent(Setup.class) 
		        		 && m.getAnnotation(Setup.class).value().equals(current)) {
		             m.invoke(null);
		         }
		      }
	   }
   }
}