from importlib.metadata import PackageNotFoundError, version

try:
    __version__ = version("aerobulk-python")
except PackageNotFoundError:
    __version__ = "unknown"
    pass

from .flux import flux_noskin, flux_skin
