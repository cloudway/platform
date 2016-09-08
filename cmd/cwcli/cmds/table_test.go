package cmds

import "os"

func ExampleTable() {
	tab := NewTable("Id", "Name", "Email", "Investments")
	tab.AddRow("231", "Albert Master", "albert.master@gmail.com", "Bonds")
	tab.AddRow("210", "Alfred Alan", "aalan@gmail.com", "Stocks")
	tab.AddRow("256", "Alison Smart", "asmart@biztalk.com", "Stocks")
	tab.AddRow("211", "Ally Emery", "allye@easymail.com", "Stocks")
	tab.Display(os.Stdout, 2)

	// Output:
	// Id   Name           Email                    Investments
	// 231  Albert Master  albert.master@gmail.com  Bonds
	// 210  Alfred Alan    aalan@gmail.com          Stocks
	// 256  Alison Smart   asmart@biztalk.com       Stocks
	// 211  Ally Emery     allye@easymail.com       Stocks
}
