import pandas as pd
import matplotlib.pyplot as plt


df = pd.read_csv("saida-1-12694394.txt", sep="  ")

# Cria uma nova figura para garantir que plots não se misturem
fig, ax = plt.subplots(figsize=(12, 8))

ax.plot(df['dimensão'], df['r^3/vol'], label=r'Linha $r^3/\mathrm{volume}$')
ax.scatter(df['dimensão'], df['r^3/vol'], color='red', label=r'Pontos $r^3/\mathrm{volume}$')

ax.set_title(r'Relação entre dimensão e $r^3/\mathrm{volume}$')
ax.set_xlabel(r'Dimensão')
ax.set_ylabel(r'$r^3/\mathrm{volume}$')
ax.legend()
ax.grid(True)

# Salva a figura
fig.savefig('saida-1-12694394.png', dpi=600, bbox_inches='tight')

plt.show()

# nova figura
fig2, ax2 = plt.subplots(figsize=(12, 8))


ax2.plot(df['dimensão'], df['abs(r^3 - vol)'], label=r'Linha $|r^3 - \mathrm{volume}|$')
ax2.scatter(df['dimensão'], df['abs(r^3 - vol)'], color='green', label=r'Pontos $|r^3 - \mathrm{volume}|$')

ax2.set_title(r'Relação entre dimensão e $|r^3 - \mathrm{volume}|$')
ax2.set_xlabel(r'Dimensão')
ax2.set_ylabel(r'$|r^3 - \mathrm{volume}|$')
ax2.legend()
ax2.grid(True)

fig2.savefig('saida-2-12694394.png', dpi=600, bbox_inches='tight')

plt.show()
