function RenderTest() {
  console.log('Vue sub-component render test');
  return false;
}
function App(props) {
  return (
    <div className='flexParent'>
      <div className='sidebar debug'>
        <SidebarPresenter model={props.model} />
      </div>
      <div className='mainContent debug'>
        <Show hash='#search'>
          <SearchPresenter model={props.model} />
        </Show>
        <Show hash='#summary'>
          <SummaryPresenter model={props.model} />
        </Show>
        <Show hash='#details'>
          <DetailsPresenter model={props.model} />
        </Show>
      </div>
      <RenderTest />
    </div>
  );
}

function defaultRoute() {
  let knownRoutes = ['#search', '#summary', '#details'];

  if (!knownRoutes.find((route) => route === window.location.hash)) {
    window.location.hash = '#search';
  }
}

defaultRoute();
