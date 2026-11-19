
from ..abstract import mixin, abstractmethod
from ..tools    import OptionProcessor, debug

import click
from click.decorators import t, _AnyCallable, GrpType
from functools        import wraps

#---------------------------------
class Group( click.Group ):
#---------------------------------
    optionKeys  = { 'expert': ('--expert',),
                    'mixin' : ('-m', '--mixin') }
    knownMixins = {}

    def parse_args( self, ctx, args ):
        obj    = ctx.ensure_object( dict )
        mixins = obj.setdefault( 'mixins', [] )
        others = []
        itr    = iter(args)
        for arg in itr:
            if arg == '--':
                others.extend( ('--', *itr) )
                break
            elif arg in self.optionKeys['expert']:
                obj['expert'] = True
            elif arg in self.optionKeys['mixin']:
                try:
                    mixins.append( self.knownMixins[next(itr)] )
                except StopIteration:
                    raise click.BadParameter(f"Missing value for {arg}")
                except KeyError as e:
                    raise click.BadParameter(f"Unknown {arg} {e.args[0]}")
            else:
                 others.append( arg )
        return super(Group, self).parse_args( ctx, others )


def group( cls: type[click.Group] = click.Group, mixins = {}, **kwArgs ) -> Group | t.Callable[[_AnyCallable], Group | GrpType]:
    kwArgs['cls'] = type(cls)( 'Group', (Group, cls), {'knownMixins': mixins} )

    @wraps(click.group)
    def decorator( func ):
        func = click.option( *Group.optionKeys['expert'], is_flag=True, help='Show all available options' )( func )
        if mixins:
            func = click.option( *Group.optionKeys['mixin'], type=click.Choice( mixins.keys(), case_sensitive=True ), multiple=True, help='Apply mixin class' )( func )
        return click.group( **kwArgs )( func )
    return decorator


@mixin
#----------------------------------------
class Controllable( OptionProcessor ):
#----------------------------------------
    __opts__ = dict( rootId = '{classId}' )
    __info__ = dict( rootId = "root scope id of Controllable type" )

    def _get_about( self ) -> dict:
        return dict( super(Controllable, self)._get_about()
                   , id     = self._id
                   , rootId = self.opts.rootId.format( classId=type( self ).__name__ )
                   )


    def __init__( self, *args, **kwArgs ):
        super(Controllable, self).__init__( *args, **kwArgs )
        self._cloneCnt = 0
        self._id       = 0
        self.initialize()


    def __getstate__( self ):
        self._cloneCnt += 1
        clone = super(Controllable, self).__getstate__()
        clone['_cloneCnt'] = 0
        clone['_id']       = self._cloneCnt
        if self.opts.debug > 0: debug()
        return clone


    def __setstate__( self, d ):
        self.__dict__.update( d )
        if self.opts.debug > 0: debug()
        self.initialize()


    def initialize( self, **kwArgs ):
        """triggers initialization of Controller instance.
        This is called implicitly by constructor and after deserialization.
        """
        rootId = self.opts.rootId.format( **self.about )
        self.__initialize__( rootId )


    # methods to be [re-]implemented by subclasses
    @abstractmethod
    def __initialize__( self, rootId ):
        """Should do initialization of state etc. using rootId."""
        ...
