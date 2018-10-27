import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.GridLayout;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Vector;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.DefaultTableModel;

import jess.JessException;
import jess.QueryResult;
import jess.ValueVector;


public class Template extends JFrame{
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	JTable table = new JTable();
	JButton btnClose = new JButton("Close");
	Vector<Vector<Object>> data = new Vector<Vector<Object>>();
	Vector<String> columns = new Vector<String>();

	JPanel content_panel = new JPanel(new FlowLayout(FlowLayout.CENTER, 30, 30));
	JScrollPane pane = new JScrollPane(table);
	String[] column_strings = {"No.","name","hobby", "height", "income", "match-rate"};

	final Template frame = this;
	
	public void initComponents(){
		getContentPane().setLayout(new BorderLayout());
		
		JLabel lblTitle = new JLabel("Matches Found!", JLabel.CENTER);
		lblTitle.setFont(new Font(Font.MONOSPACED, Font.BOLD, 21));
		getContentPane().add(lblTitle, BorderLayout.NORTH);
		
		JPanel left_panel = new JPanel(new BorderLayout());
		JLabel lblHeader = new JLabel("Your Profile");
		lblHeader.setFont(new Font(Font.MONOSPACED, Font.BOLD, 15));
		left_panel.add(lblHeader, BorderLayout.NORTH);
		
		//Panel that contain all user's info
		JPanel grid_panel = new JPanel(new GridLayout(7, 2));
		
		JLabel lblName = new JLabel("Name : ");
		JLabel lblAge = new JLabel("Age : ");
		JLabel lblGender = new JLabel("Gender : ");
		JLabel lblHeight = new JLabel("Height  : ");
		JLabel lblHobby = new JLabel("Hobby  : ");
		JLabel lblInterest = new JLabel("Interest : ");
		JLabel lblIncome = new JLabel("Prefered Income : ");
		
		//Labels that contain user's info
		JLabel lblNameInfo = new JLabel();
		JLabel lblAgeInfo = new JLabel();
		JLabel lblGenderInfo = new JLabel();
		JLabel lblHeightInfo = new JLabel();
		JLabel lblHobbyInfo = new JLabel();
		JLabel lblInterestInfo = new JLabel();
		JLabel lblIncomeInfo = new JLabel();
		
		//Object that can be used as panel that contain image or table
		Object panel_add = null;
		
		//No match found
		panel_add = imageNotAvailable();
		
		initTable();
		/*Fill the code here to fetch all suitable match for user
		
		*/
		try {
			QueryResult result_male = Main.engine.runQueryStar("GetMaleResult", new ValueVector());
			QueryResult result_female = Main.engine.runQueryStar("GetFemaleResult", new ValueVector());
					
			if(result_male != null){
				while(result_male.next()){
					columns.add(result_male.getString("numMale"));
				}
			}
			else{
				while(result_female.next()){
					columns.add(result_female.getString("numFemale"));
				}				
			}
		} catch (JessException e) {
			e.printStackTrace();
		}
		
		grid_panel.add(lblName);
		grid_panel.add(lblNameInfo);
		
		grid_panel.add(lblAge);
		grid_panel.add(lblAgeInfo);
		
		grid_panel.add(lblGender);
		grid_panel.add(lblGenderInfo);
		
		grid_panel.add(lblHeight);
		grid_panel.add(lblHeightInfo);
		
		grid_panel.add(lblHobby);
		grid_panel.add(lblHobbyInfo);
		
		grid_panel.add(lblInterest);
		grid_panel.add(lblInterestInfo);
		
		grid_panel.add(lblIncome);
		grid_panel.add(lblIncomeInfo);
		
		left_panel.add(grid_panel, BorderLayout.CENTER);
		
		content_panel.add(left_panel);
		content_panel.add((Component) panel_add);
		content_panel.setPreferredSize(new Dimension (800, 450));
		
		getContentPane().add(content_panel, BorderLayout.CENTER);
		getContentPane().add(btnClose, BorderLayout.PAGE_END);
		
		btnClose.addActionListener(new ActionListener() {
			
			@Override
			public void actionPerformed(ActionEvent arg0) {
				frame.dispose();
			}
		});
	}
	
	private Image getScaledImage(Image srcImage, int width, int height)
	{
		BufferedImage resizedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
		Graphics2D g2d = resizedImage.createGraphics();
		
		g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);
		g2d.drawImage(srcImage, 0, 0, width, height, null);
		g2d.dispose();
		
		return resizedImage;
	}
	
	public JLabel imageNotAvailable(){
		JLabel lbl_img = new JLabel();
		lbl_img.setPreferredSize(new Dimension(320,180));
		Image bufferedImage;
		try {
			bufferedImage = ImageIO.read(getClass().getResource("not_available.jpg"));
			ImageIcon icon = new ImageIcon(getScaledImage(bufferedImage, 320, 180));
			lbl_img.setIcon(icon);
		} catch (IOException e) {
			return null;
		}
		return lbl_img;
	}
	
	public void initTable(){
		columns.clear();
		data.clear();
		for (String s : column_strings) {
			columns.add(s);
		}
	}
	
	public Template() {
		setTitle("The Result of Consultation");
		setSize(850, 450);
		setLocationRelativeTo(null);
		setDefaultCloseOperation(EXIT_ON_CLOSE);
		initComponents();
		setResizable(false);
		setVisible(true);
	}
}
