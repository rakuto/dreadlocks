Welcome to DreadLocks
=====================

The DreadLocks is new template engine for Scala programmers, and supported
scala syntax in your template files.

Getting Started
---------------
1. Install DreadLocks

Download latest dreadlocks.jar from GitHub:
http://cloud.github.com/downloads/rakuto/dreadlocks/dreadlocks-0.2.jar

Features
--------
- Support Scala expression
    
    Currently support limited Scala expression, foreach, if-elseif-else, to apply method and more.

Goals
-----
- fast

    The DreadLocks aims to be fastest template engine.

- easy to use

    Support all Scala expression. 

Build
-----
DreadLocks used simple-build-tool project to build source and manage it. You can
build after installed and setuped simple-build-tools. The simple-build-tool project are 
hosted on Google Code <http://code.google.com/p/simple-build-tool/>.

% sbt compile
% sbt package

For more details, please see simple-build-tool documentation.

ToDo
-----
* Optimize and Speed up!!
* Nested tamplates
* Caching
* More support of expression of Scala
* Better error message

Examples
--------

You should to import DreadLocks code at the top on your program.

    import org.scalalites.dreadlocks._

The DreadLocks temlate engine allows ${...} and #{...} syntax to evaluate the variable, and
<?sc ... ?> syntax is evaluated as statements, like foreach, if-else etc. You can see some 
examples below. There are many exmaples in src/test/resources/ and src/test/scala/, please see them.

template_variable.shtml:
    <html>
        <head>
            <title>${title}</title>
        </head>
        <body>
            #{body}
        </body>
    </html>

TemplateVariable.scala:
    // You can specify instance of java.io.File, scala.io.Source or String to constructor of Template class.
    val input = Source.fromFile("template_variable.shtml").getLines.mkString
    val template = Template(input)
    val context = Context("title" -> "Welcome to DreadLocks", "body" -> "DreadLocks project are hosted on scalalites.org.")
    val output = template.render(context)

    Console.println(output)
    // <html>
    //    <head>
    //        <title>Welcome to DreadLocks</title>
    //    </head>
    //    <body>DreadLocks project are hosted on scalalites.org.</body>
    // </html>

ex. Examples that evaluate iterable variables and if-else statement.

hamburgers.txt
    Delicious hamburger restraunts in Japan:
    <?sc restraunts.foreach { restraunt => ?>
        * ${restraunt} 
    <?sc } ?>

    Side menus:
    <?sc if(haveSideMenu) { ?>
        * ${sideMenus("potato")}
        * ${sideMenus("coleslaw")}
    <?sc } ?>

Complex.scala
    // You can specify instance of java.io.File, scala.io.Source or String to constructor of Template class.
    val source   = Source.fromFile("template_variable.shtml")
    val template = Template(source)
    val context  = Context("restraunts" -> List("Baker Bounce", "Great Burger", "FELLOWS"), 
                           "haveSideMenu" -> true, 
                           "sideMenus" -> HashMap("potato" -> "French fries", "coleslaw" -> "Coleslaw"))

    val output = template.render(context)
    // Console.println(output)
    // Delicious hamburger restraunts in Japan:
    //   * Baker Bounce
    //   * Great Burger
    //   * FELLOWS
    //
    // Side menus:
    //   * French fries
    //   * Coleslaw
