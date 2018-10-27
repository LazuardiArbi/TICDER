import jess.JessException;
import jess.Rete;

public class Main {
	public static Rete engine = new Rete();
	
	public static void main(String[] args) {
		try {
			engine.batch("Project.clp");
			engine.reset();
			engine.run();
		} catch (JessException e) {
			e.printStackTrace();
		}
	}
}
