from matplotlib.colors import ListedColormap, BoundaryNorm
from windrose import WindroseAxes,WindAxes
import numpy as np
import matplotlib.pyplot as plt

def create_windrose_figure(df, mosaic, titles=None, gridspec_kw=None):
    """
    Create a figure with windrose plots for distance and bearing data.
    
    Parameters:
        df (pd.DataFrame): Input data containing '.dist' and '.bear' columns
        mosaic (list): Subplot mosaic layout
        titles (list): Titles for each subplot (default: ['Centroids', 'Naive Medians', 'Geometric Medians'])
        gridspec_kw (dict): Grid specification parameters
    """
    # Initialize parameters with defaults
    titles = titles or ['Centroids', 'Naive Medians', 'Geometric Medians']
    gridspec_kw = gridspec_kw or {}
    
    # Create figure with constrained layout
    fig = plt.figure(figsize=(8, 5), dpi=300, layout="constrained")
    
    # Prepare subplot configuration
    plot_keys = [x for y in mosaic for x in y if x not in [None, 'cbar']]
    subplot_kw = {tuple(plot_keys): {'projection': 'windrose'}}
    
    # Create subplots using mosaic layout
    ax_dict = fig.subplot_mosaic(
        mosaic,
        empty_sentinel=None,
        gridspec_kw=gridspec_kw,
        per_subplot_kw=subplot_kw
    )
    
    # Get column names (removing '.dist' suffix)
    distance_columns = [col for col in df.columns if col.endswith('.dist')]
    base_columns = [col.rsplit('.', 1)[0] for col in distance_columns]
    
    # Calculate bins for distance data
    min_distance = df[distance_columns].min().min()
    max_distance = df[distance_columns].max().max()
    bins = calculate_distance_bins(max_distance)
    
    # ============= ESSENTIAL: Calculate consistent y-axis limits for all plots
    y_axis_limit = calculate_max_y_axis_limit(df, base_columns)
    # =============
    
    # Create windrose plots for each column
    for idx, column in enumerate(base_columns):
        plot_windrose_subfigure(
            ax_dict[column],
            df[f'{column}.bear'],
            df[f'{column}.dist'],
            bins,
            titles[idx],
            y_axis_limit
        )
    
    # Add colorbar
    add_colorbar(ax_dict['cbar'], bins)
    
    return fig

def calculate_distance_bins(m):
    """6-7 bins in multiples of 5 that tightly fit 0-max_distance."""
    s = max(5, round(m/6/5)*5)  # step size
    b = np.arange(0, (int(m//s)+2)*s, s)  # bins
    return b[:7] if len(b) > 7 else b  # cap at 7 bins

def calculate_max_y_axis_limit(df, columns):
    """Calculate maximum y-axis limit across all windrose plots."""
    max_y = 0
    for column in columns:
        distances = df[f'{column}.dist']
        bearings = df[f'{column}.bear']
        ax = Plot.plot_windrose_subplot(
            ax=None, 
            bearings=bearings, 
            distances=distances, 
            plot_type='bar', 
            cmap='viridis'
        )
        max_y = max(np.ceil(ax._info['table'].sum(axis=0).max()), max_y)
    return int(np.ceil(max_y / 10) * 10)  # Round to next multiple of 10


def plot_windrose_subplot(ax, bearings, distances, **kwargs):

  # Read kwargs
  cmap = plt.get_cmap(kwargs.get('cmap','Greys_r'))
  bins = kwargs.get('bins',np.linspace(min(distances), max(distances), 6))
  plot_type = kwargs.get('plot_type','contourf')

  # Make an axis object if it does not exist
  if ax is None:
      fig = plt.figure(figsize=(6, 6))
      ax = WindroseAxes.from_ax(fig=fig)
      ax.yaxis.grid(zorder=0)
  else:
      ax = ax
      ax.yaxis.grid(zorder=0)


  if plot_type == 'contourf':
    ax.contourf(bearings, distances, bins=bins, normed=True, cmap=cmap)
  elif plot_type == 'bar':
    ax.bar(bearings, distances, bins=bins, normed=True, cmap=cmap,nsector=8)
  
  ax.set_title(kwargs.get('title',''))

  #return ax

def plot_windrose_subfigure(ax, bearings, distances, bins, title, y_limit):
    """Create a single windrose subplot with consistent styling."""
    # Plot the windrose
    plot_windrose_subplot(
        ax=ax,
        bearings=bearings,
        distances=distances,
        bins=bins,
        plot_type='bar',
        cmap='viridis'
    )
    
    # Set title and formatting
    ax.set_title(title, ha='center', va='bottom', fontweight='bold', x=0.5, y=1.15)
    ax.set_ylim(0, y_limit)
    ax.set_yticks(np.arange(0, y_limit + 10, 10))
    ax.tick_params(labelleft=False)
    
    # Add grid lines
    ax.yaxis.grid(linewidth=0.1, color='#cfcfcf')
    ax.xaxis.grid(linewidth=0.1, color='#cfcfcf')
    
    # Add scale bar
    y_ticks = list(np.arange(0, y_limit + 10, 10))
    x_position = -np.pi/8  # in radians
    ax.plot([x_position]*len(y_ticks), y_ticks, 0, marker='.', color='#636363', zorder=5)
    ax.text(x_position, y_ticks[-1] + 4, str(y_ticks[-1]),
           zorder=5, ha='left', va='bottom', color='#636363')


def add_colorbar(cbar_ax, bins, aspect=20):
    """Add colorbar to figure with adjustable relative dimensions.
    """
    cmap = ListedColormap(plt.cm.viridis(np.linspace(0, 1, len(bins) - 1)))
    norm = BoundaryNorm(bins, cmap.N)
    sm = plt.cm.ScalarMappable(cmap=cmap, norm=norm)
    sm.set_array([])  # Necessary for colorbar to work
    
    cbar = plt.colorbar(sm, cax=cbar_ax, ticks=bins)
    cbar.set_label('Distance (km)', rotation=270, labelpad=15)
    
    # Adjust colorbar proportions
    cbar_ax.set_aspect(aspect) 
