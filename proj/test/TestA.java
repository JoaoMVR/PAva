public class TestA {
	public static void main(String[] args) {
		System.out.println(new Widget());
		System.out.println(new Widget("width", 80));
		System.out.println(new Widget("height", 30));
		System.out.println(new Widget("margin", 2));
		System.out.println(new Widget("width", 8, "height", 13, "margin", 21));
	}
}
