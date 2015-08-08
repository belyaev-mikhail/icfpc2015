using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;

namespace WindowsFormsApplication1
{
    struct Cell
    {
        public int column;
        public int row;
        public Color? filled;

        public Cell(int i, int j, Color? isFULL) : this()
        {
            this.row = i;
            this.column = j;
            this.filled = isFULL;
           
        }

        public override string ToString()
        {
            return String.Format("Cell({0}, {1}, {2})", row, column, filled);
        }
    }
}
