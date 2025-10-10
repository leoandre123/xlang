using xlang.Compiler.Structures.AST;

namespace xlang.Compiler.Utils;

public static class AstPrinter
{

    private static void WriteTreeMessage(string? label, object msg, List<bool> hasSiblings, ConsoleColor color)
    {
        for (var i = 0; i < hasSiblings.Count - 1; i++)
        {
            Console.Write(hasSiblings[i] ? "│   " : "    ");
        }

        if (hasSiblings.Count != 0)
        {
            if (hasSiblings.Last())
            {
                Console.Write("├───");
            }
            else
            {
                Console.Write("└───");
            }

        }

        if (label != null)
        {
            Console.Write(label + ": ");
        }
        Console.ForegroundColor = color;
        Console.WriteLine(msg);
        Console.ForegroundColor = ConsoleColor.White;
    }

    public static void PrintAST(Node node, List<bool> siblings = null)
    {
        siblings ??= [];

        //void PrintLine(object msg, bool isLast)
        //{
        //    WriteTreeMessage(msg, [.. siblings, !isLast], ConsoleColor.Blue);
        //}

        void PrintChild(Node child, bool isLast)
        {
            PrintAST(child, [.. siblings, !isLast, false]);
        }

        void PrintList(IReadOnlyList<Node> list, bool isLast)
        {
            for (var i = 0; i < list.Count; i++)
                PrintAST(list[i], [.. siblings, !(i == list.Count - 1 && isLast)]);
        }

        void PrintLabeledList(string label, IReadOnlyList<Node> list, bool isLast)
        {
            WriteTreeMessage(label, "", [.. siblings, !isLast], ConsoleColor.Blue);
            for (var i = 0; i < list.Count; i++)
                PrintAST(list[i], [.. siblings, !isLast, i != list.Count - 1]);
        }

        void PrintLabeledChild(string label, Node child, bool isLast)
        {
            WriteTreeMessage(label, "", [.. siblings, !isLast], ConsoleColor.Blue);
            PrintAST(child, [.. siblings, !isLast, false]);
        }

        void PrintLabeledValue(string label, object value, bool isLast)
        {
            WriteTreeMessage(label, value, [.. siblings, !isLast], ConsoleColor.Blue);
        }



        WriteTreeMessage(null, node.GetType().Name, siblings, ConsoleColor.Green);

        switch (node)
        {
            case Module module:
                PrintLabeledValue("File", module.File, false);
                PrintLabeledChild("Program", module.Program, true);
                break;
            case ClassDeclaration classDeclaration:
                PrintLabeledValue("Name", classDeclaration.Name, false);
                PrintLabeledList("Members", classDeclaration.Members, true);
                break;
            case ArrayInitializerListExpression arrayInitializerListExpression:
                PrintList(arrayInitializerListExpression.Elements, true);
                break;
            case BinaryExpression binaryExpression:
                PrintLabeledValue("Operator", binaryExpression.Operator.Symbol, false);
                PrintLabeledChild("Left", binaryExpression.Left, false);
                PrintLabeledChild("Right", binaryExpression.Right, true);
                break;
            case Block block:
                PrintList(block.Statements, true);
                break;
            case BooleanLiteralExpression booleanLiteralExpression:
                PrintLabeledValue("Value", booleanLiteralExpression.Value, true);
                break;
            case CallExpression callExpression:
                PrintLabeledValue("Name", callExpression.Name, false);
                PrintLabeledList("Arguments", callExpression.Arguments, true);
                break;
            case ArrayDeclaration arrayDeclaration:
                PrintLabeledValue("Name", arrayDeclaration.Name, false);
                PrintLabeledValue("Size", arrayDeclaration.Size, arrayDeclaration.Initial == null);
                if (arrayDeclaration.Initial != null)
                    PrintLabeledChild("Inital value", arrayDeclaration.Initial, true);
                break;
            case ClassField classField:
                PrintLabeledValue("Name", classField.Name, false);
                PrintLabeledValue("Type", classField.Type, classField.Initial == null);
                if (classField.Initial != null)
                    PrintLabeledChild("Inital value", classField.Initial, true);
                break;
            case FloatLiteralExpression floatLiteralExpression:
                PrintLabeledValue("Value", floatLiteralExpression.Value, true);
                break;
            case IndexExpression indexExpression:
                PrintLabeledChild("Base", indexExpression.Base, false);
                PrintLabeledChild("Index", indexExpression.Index, true);
                break;
            case IntegerLiteralExpression integerLiteralExpression:
                PrintLabeledValue("Value", integerLiteralExpression.Value, true);
                break;
            case MemberAccessExpression memberAccessExpression:
                PrintLabeledValue("Member", memberAccessExpression.Member, false);
                PrintLabeledChild("Base", memberAccessExpression.Base, true);
                break;
            case MemberCallExpression memberCallExpression:
                PrintLabeledValue("Member", memberCallExpression.Member, false);
                PrintLabeledChild("Base", memberCallExpression.Base, false);
                PrintLabeledValue("Argument", memberCallExpression.Arguments, true);
                break;
            case StringInterpolationExpression stringInterpolationExpression:
                break;
            case StringLiteralExpression stringLiteralExpression:
                break;
            case UnaryExpression unaryExpression:
                PrintLabeledValue("Operator", unaryExpression.Operator.Symbol, false);
                PrintLabeledChild("Left", unaryExpression.Operand, true);
                break;
            case VariableExpression variableExpression:
                PrintLabeledValue("Name", variableExpression.Name, true);
                break;
            case ForStatement forStatement:
                PrintLabeledChild("Init", forStatement.Initial, false);
                PrintLabeledChild("Condition", forStatement.Expression, false);
                PrintLabeledChild("Increment", forStatement.Increment, false);
                PrintLabeledChild("Body", forStatement.Body, false);
                break;
            case FunctionAttribute functionAttribute:
                break;
            case FunctionDeclaration functionDeclaration:
                PrintLabeledValue("Name", functionDeclaration.Name, false);
                PrintLabeledList("Params", functionDeclaration.Parameters, functionDeclaration.IsExtern);
                if (!functionDeclaration.IsExtern)
                {
                    PrintLabeledChild("Body", functionDeclaration.Body!, true);
                }
                break;
            case FunctionParameter functionParameter:
                PrintLabeledValue("Name", functionParameter.Name, false);
                PrintLabeledValue("Type", functionParameter.Type, true);
                break;
            case IfStatement ifStatement:
                PrintLabeledChild("Condition", ifStatement.Expression, false);
                PrintLabeledChild("Body", ifStatement.Body, true);
                break;
            case ProgramDeclaration programDeclaration:
                PrintLabeledList("Imports", programDeclaration.Imports, false);
                PrintLabeledChild("Body", programDeclaration.Body, true);
                break;
            case ProgramImport programImport:
                PrintLabeledValue("Scope", programImport.Scope, true);
                break;
            case Return returnStatement:
                if (returnStatement.Expression != null)
                    PrintLabeledChild("Expression", returnStatement.Expression, true);
                break;
            case ScopeBody scopeBody:
                PrintList([.. scopeBody.Aliases, .. scopeBody.Functions, .. scopeBody.Classes, .. scopeBody.Scopes], true);
                break;
            case ScopeDeclaration scopeDeclaration:
                PrintLabeledValue("Name", scopeDeclaration.Name, false);
                PrintLabeledChild("Body", scopeDeclaration.Body, true);
                break;
            case VariableDeclaration variableDeclaration:
                PrintLabeledValue("Name", variableDeclaration.Name, false);
                PrintLabeledValue("Type", variableDeclaration.Type, variableDeclaration.Initial == null);
                if (variableDeclaration.Initial != null)
                {
                    PrintLabeledChild("Initial expression", variableDeclaration.Initial, true);
                }

                break;
            case WhileStatement whileStatement:
                PrintLabeledChild("Condition", whileStatement.Expression, false);
                PrintLabeledChild("Body", whileStatement.Body, true);
                break;
            case AliasDeclaration typeDeclaration:
                PrintLabeledValue("Name", typeDeclaration.Name, false);
                PrintLabeledValue("Type", typeDeclaration.Type, true);
                break;
            default:
                throw new ArgumentOutOfRangeException(nameof(node));
        }

    }

}
