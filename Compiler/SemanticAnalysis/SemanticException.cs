using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace xlang.Compiler.SemanticAnalysis;

public class SemanticException(string message, SourceSpan span, string sourceFile) : CompilationException(message, span, sourceFile);
