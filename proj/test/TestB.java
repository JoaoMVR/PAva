import ist.meic.pa.test.classes.*;

public class TestB {
	public static void main(String[] args) {
		System.out.println(new ExtendedWidget());
		System.out.println(new ExtendedWidget("width", 80));
		System.out.println(new ExtendedWidget("height", 30));
		System.out.println(new ExtendedWidget("height", 20, "width", 90));
		System.out.println(new ExtendedWidget("height", 20, "width", 90, "name", "Nice"));
	}
}
