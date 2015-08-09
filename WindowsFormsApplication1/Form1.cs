using Microsoft.FSharp.Core;
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
            var res = DataPart.doit(argv).First().First();
            field = res.Item3;
            unitQueue = res.Item4;

            //MessageBox.Show(DataPart.printField(field));
            for (var i = 0; i < field.width; ++i)
            {
                for(var j = 0; j < field.height; ++j)
                {
                    var c = DataPart.getCell(i, j, field);
                    cells.Add(new Cell(j, i, c.IsFULL? Color.Red : ((Color?)null)));
                }
            }

            nextUnit();
            
            //var unit = units.First();
            //cells.Add(new Cell(unit.pivot.col, unit.pivot.row, Color.Aquamarine));
            //foreach(var f in unit.filled)
            //{
            //    cells.Add(new Cell(f.row, f.col, Color.BurlyWood));
            //}

            this.Width = field.width * 25 + 75;
            this.Height = field.height * 18 + 72;

            updateTimer = new Timer();
            updateTimer.Tick += UpdateTimer_Tick;
        }

        private void UpdateTimer_Tick(object sender, EventArgs e)
        {
            unit = aiMoves.FirstOrDefault();
            if (unit == null)
            {
                updateTimer.Stop();
                MessageBox.Show("No more AI moves");

                return;
            }

            aiMoves = aiMoves.Skip(1);

            if (aiMoves.FirstOrDefault() == null)
            {
                //aiMoves = aiMoves.Skip(1);
                handleLock();
            }

            genUnitCells();
            pictureBox1.Refresh();
        }

        Timer updateTimer;
        List<Cell> cells = new List<Cell>();

        Cell? unitPivot;
        List<Cell> unitCells = new List<Cell>();
        IEnumerable<DataPart.Unit> unitQueue;
        IEnumerable<DataPart.Unit> aiMoves;
        DataPart.Unit unit;
        DataPart.Field field;

        private void nextUnit()
        {
            unit = unitQueue.FirstOrDefault();
            if(unit == null)
            {
                updateTimer.Stop();
                MessageBox.Show("Out of pieces; technically, you won!");
                
                return;
            }
            unitQueue = unitQueue.Skip(1);

            unit = DataPart.unit_start(unit, field);

            var bm = DataPart.best_move(unit, field);

            aiMoves = bm.Select(pair => pair.Item2);

            //MessageBox.Show(DataPart.string_from_units(bm.Select(pair => pair.Item1)));

            genUnitCells();
        }

        private void genUnitCells()
        {
            if (unit == null) return;
            unitPivot = new Cell(unit.pivot.row, unit.pivot.col, Color.Aquamarine);
            unitCells.Clear();
            foreach(var f in unit.filled)
            {
                unitCells.Add(new Cell(f.row, f.col, Color.BurlyWood));
            }
        }

        private void genFieldCells()
        {
            cells.Clear();
            for (var i = 0; i < field.width; ++i)
            {
                for (var j = 0; j < field.height; ++j)
                {
                    var c = DataPart.getCell(i, j, field);
                    cells.Add(new Cell(j, i, c.IsFULL ? Color.Red : ((Color?)null)));
                }
            }
        }

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
                init.Inflate(2, 5);
                e.Graphics.FillEllipse(new SolidBrush(Color.Black), init);

                Rectangle final = cellRect(col, rows + 1);
                final.Inflate(2, 5);
                e.Graphics.FillEllipse(new SolidBrush(Color.Black), final);
            }

            for (int row = 0; row <= rows + 1; ++row)
            {
                Rectangle init = cellRect(0, row);
                init.Inflate(2, 5);
                e.Graphics.FillEllipse(new SolidBrush(Color.Black), init);

                Rectangle final = cellRect(cols + 1, row);
                final.Inflate(2, 5);
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

            for (int x = 0; x <= unitCells.Count - 1; x++)
            {
                Rectangle r = cellRect(unitCells[x].column + 1, unitCells[x].row + 1);
                if (unitCells[x].filled != null)
                {
                    r.Inflate(0, 3);
                    e.Graphics.FillEllipse(new SolidBrush(unitCells[x].filled.Value), r);
                }
            }

            if (unitPivot != null)
            {
                Rectangle r = cellRect(unitPivot.Value.column + 1, unitPivot.Value.row + 1);
                if (unitPivot.Value.filled != null)
                {
                    r.Inflate(-8, -4);
                    e.Graphics.FillEllipse(new SolidBrush(unitPivot.Value.filled.Value), r);
                }
            }

        }

        private void handleLock()
        {
            var apply = DataPart.apply_unit(unit, field);
            if(FSharpOption<DataPart.Field>.get_IsNone(apply))
            {
                MessageBox.Show("You lost!");
                updateTimer.Stop();
                return;
            }
            field = apply.Value;
            field = DataPart.handle_lock(field);
            genFieldCells();
            nextUnit();
        }

        private void Form1_KeyDown(object sender, KeyEventArgs e)
        {
            if (unit == null) return;

            DataPart.Unit unit_ = null;
            if (e.KeyCode == Keys.D && !e.Control)
            {
                unit_ = DataPart.apply_move_zipped(DataPart.Move.NewMShift(DataPart.ShiftDirection.E), unit);
            }
            else if (e.KeyCode == Keys.A && !e.Control)
            {
                unit_ = DataPart.apply_move_zipped(DataPart.Move.NewMShift(DataPart.ShiftDirection.W), unit);
            }
            else if (e.KeyCode == Keys.D && e.Control || e.KeyCode == Keys.C)
            {
                unit_ = DataPart.apply_move_zipped(DataPart.Move.NewMShift(DataPart.ShiftDirection.SE), unit);
            }
            else if (e.KeyCode == Keys.A && e.Control || e.KeyCode == Keys.Z)
            {
                unit_ = DataPart.apply_move_zipped(DataPart.Move.NewMShift(DataPart.ShiftDirection.SW), unit);
            }
            else if (e.KeyCode == Keys.Q)
            {
                unit_ = DataPart.apply_move_zipped(DataPart.Move.NewMRotate(DataPart.RotateDirection.CCW), unit);
            }
            else if (e.KeyCode == Keys.E)
            {
                unit_ = DataPart.apply_move_zipped(DataPart.Move.NewMRotate(DataPart.RotateDirection.CW), unit);
            } else if(e.KeyCode == Keys.Space)
            {
                unit_ = aiMoves.First();
                aiMoves = aiMoves.Skip(1);
            }
            else if (e.KeyCode == Keys.Enter)
            {
                updateTimer.Interval = 40;
                updateTimer.Enabled = true;
                updateTimer.Start();
                return;
            }
            else return;

            if (DataPart.valid_zipped(field, unit_))
            {
                unit = unit_;
            }
            else handleLock();

            if (aiMoves.FirstOrDefault() == null)
            {
                //aiMoves = aiMoves.Skip(1);
                handleLock();
            }


            genUnitCells();
            pictureBox1.Refresh();
        }

        private void Form1_Load(object sender, EventArgs e)
        {

        }
    }
}
