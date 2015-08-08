using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace WindowsFormsApplication1
{
    public partial class Form1 : Form
    {
        public Form1(string[] argv)
        {
            InitializeComponent();
            var res = DataPart.doit(argv).First();
            var dp = res.Item1;
            var units = res.Item2;
            MessageBox.Show(DataPart.printField(dp));
            for (var i = 0; i < dp.width; ++i)
            {
                for(var j = 0; j < dp.height; ++j)
                {
                    var c = DataPart.getCell(i, j, dp);
                    cells.Add(new Cell(j, i, c.IsFULL? Color.Red : ((Color?)null)));
                }
            }

            //var unit = units.First();
            //cells.Add(new Cell(unit.pivot.col, unit.pivot.row, Color.Aquamarine));
            //foreach(var f in unit.filled)
            //{
            //    cells.Add(new Cell(f.row, f.col, Color.BurlyWood));
            //}

            this.Width = dp.width * 25 + 75;
            this.Height = dp.height * 18 + 72;
        }

        List<Cell> cells = new List<Cell>();

        private Rectangle cellRect(int columnIndex, int rowIndex)
        {
            return new Rectangle(cellX(columnIndex, rowIndex), cellTop(columnIndex, rowIndex) + 5, 25, 13);
        }

        private int cellX(int columnIndex, int rowIndex)
        {
            if (rowIndex % 2 == 0)
            {
                return columnIndex * 25;
            }
            else
            {
                return (columnIndex * 25) - 12;
            }
        }

        private int cellTop(int columnIndex, int rowIndex)
        {
            if (rowIndex % 2 == 0)
            {
                if (rowIndex == 0)
                {
                    return 0;
                }
                return Convert.ToInt32(rowIndex / 2) * 36;
            }
            else
            {
                return Convert.ToInt32(Math.Ceiling(Convert.ToDecimal(rowIndex) / 2) * 36 - 18);
            }
        }

        private void pictureBox1_Paint(object sender, PaintEventArgs e)
        {
            Point[] points = {
                new Point(12, 0),
                new Point(25, 5),
                new Point(25, 18),
                new Point(12, 23),
                new Point(0, 18),
                new Point(0, 5)
            };

            for (int x = 0; x <= pictureBox1.Width; x += 25)
            {
                for (int y = 0; y <= pictureBox1.Height; y += 36)
                {
                    e.Graphics.DrawPolygon(Pens.LightGray, Array.ConvertAll(points, p => new Point(p.X + x, p.Y + y)));
                    e.Graphics.DrawPolygon(Pens.LightGray, Array.ConvertAll(points, p => new Point(p.X + x - 13, p.Y + y + 18)));
                }
            }

            var cols = cells.Max(cell => cell.column) + 1;
            var rows = cells.Max(cell => cell.row) + 1;

            for (int col = 0; col <= cols + 1; ++col)
            {
                Rectangle init = cellRect(col, 0);
                init.Inflate(0, 3);
                e.Graphics.FillEllipse(new SolidBrush(Color.Black), init);

                Rectangle final = cellRect(col, rows + 1);
                final.Inflate(0, 3);
                e.Graphics.FillEllipse(new SolidBrush(Color.Black), final);
            }

            for (int row = 0; row <= rows + 1; ++row)
            {
                Rectangle init = cellRect(0, row);
                init.Inflate(0, 3);
                e.Graphics.FillEllipse(new SolidBrush(Color.Black), init);

                Rectangle final = cellRect(cols + 1, row);
                final.Inflate(0, 3);
                e.Graphics.FillEllipse(new SolidBrush(Color.Black), final);
            }

            for (int x = 0; x <= cells.Count - 1; x++)
            {
                Rectangle r = cellRect(cells[x].column + 1, cells[x].row + 1);
                if (cells[x].filled != null)
                {
                    r.Inflate(0, 3);
                    e.Graphics.FillEllipse(new SolidBrush(cells[x].filled.Value), r);
                }
            }
        }
    }
}
