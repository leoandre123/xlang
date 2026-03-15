using System.Text;

namespace xlang.Compiler.CodeGeneration;

public class AssemblyWriter
{
    //private readonly StringBuilder _sb = new();
    private readonly List<string> _lines = [];
    private int _labelId;
    private int _currentLine = -1;

    public void Append(string str, bool indented = true) => AppendLine(indented ? $"    {str}" : str);
    public void Comment(string comment)
    {
        foreach (var line in comment.Split('\n'))
        {
            AppendLine($";{line}");
        }
    }

    public void Label(string label) => AppendLine($"{label}:");
    public string CreateLabel(string label) => $"{label}_{_labelId++}";

    public override string ToString() => string.Join(Environment.NewLine, _lines);
    //public void Insert(int pos, string str, bool indented = true) => _sb.Insert(pos, indented ? $"    {str}\n" : $"{str}\n");
    public void Replace(string old, string replace)
    {
        for (var index = 0; index < _lines.Count; index++)
        {
            _lines[index] = _lines[index].Replace(old, replace);
        }
    }

    public void SetLine(int line)
    {
        _currentLine = line;
    }
    public void SetReplaceLine(string str)
    {
        var index = _lines.FindLastIndex(x => x == str);
        _currentLine = index;
        _lines.RemoveAt(index);
    }

    public void RestoreLine()
    {
        _currentLine = -1;
    }

    private void AppendLine(string str)
    {
        if (_currentLine == -1)
        {
            _lines.Add(str);
        }
        else
        {
            _lines.Insert(_currentLine++, str);
        }
    }
}